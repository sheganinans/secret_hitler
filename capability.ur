open Auth
open Server_response

fun eval_capability (arg : option capability_arg)
                    (room_id : int)
                    (cap_id  : int) : transaction {} =
    pigot <- player_in_game_on_turn_exn room_id;
    let val (rt, gt, tt, ot) = (pigot.Room, pigot.Game, pigot.Turn, pigot.TableOrder)

        val current_step = current_step tt
        val send_public_message = send_public_message gt

        fun set_rule_set {} : transaction {} =
            case arg of
                Some (RuleSetArg rs) =>
                send_public_message (RuleSet rs)
              | _ => return {}

        fun start_game {} : transaction {} = return {}

        fun choose_chancellor (chan_id : int) : transaction {} =
            submit_chancellor tt chan_id;
            send_public_message (ChancellorChosen chan_id)

        fun discard_policy (p : card) : transaction {} =
            submit_discard tt p;
            send_public_message PresidentDiscard

        fun enact_policy (c : card) : transaction {} =
            let val chosen_policy : side = deserialize (case c of Fst => tt.Fst
                                                                | Snd => tt.Snd
                                                                | Trd => tt.Trd)
            in
                send_public_message (PolicyPassed chosen_policy)
            end

        fun eval_vote (v : option bool) : transaction {} =
            vote_o <- does_vote_exist_for tt ot.Place;
            (case vote_o of None   =>    new_vote
                          | Some _ => update_vote) tt ot.Place v;
            send_public_message (VoteNotif { Place = ot.Place
                                           , Vote = Option.isSome vote_o })

        fun eval_exec_action (a : Types.exec_action) : transaction {} =
            if current_step <> ExecActionStep then return {}
            else
                let fun investigate_loyalty (place : int) : transaction {} =
                        let fun is_a_fascist (place : int) : transaction {} =
                                return {}
                            fun is_a_liberal (place : int) : transaction {} =
                                return {}
                        in  submit_loyalty_investigation tt place;
                            possible_liberal <- possible_liberal rt place;
                            (case possible_liberal of None   => is_a_fascist
                                                    | Some _ => is_a_liberal) place
                        end

                    fun call_special_election (place : int) : transaction {} =
                        return {}

                    fun execute_player (place : int) : transaction {} =
                        kill_player tt place;
                        send_public_message (PlayerExecuted place)

                    fun president_veto {} : transaction {} =
                        president_veto_turn tt;
                        send_public_message VetoEnacted

                    fun reject_veto {} : transaction {} =
                        chanc_ord <- player_at_table tt tt.Chancellor;
                        chancellor_in_game <- player_connection_in_game tt chanc_ord.InGameId;
                        send chancellor_in_game.Chan (PrivateRsp (ChancellorRsp VetoRejected))

                in  case a of
                        InvestigateLoyalty  place => investigate_loyalty   place
                      | CallSpecialElection place => call_special_election place
                      | ExecutePlayer       place => execute_player        place
                      |       Veto                => president_veto {}
                      | RejectVeto                =>    reject_veto {}
                end

        fun run_timed (a : timed_action) : transaction {} =
            update_last_action gt;
            case a of
                ChooseChancellor c => choose_chancellor c
              |    DiscardPolicy p =>    discard_policy p
              |      EnactPolicy p =>      enact_policy p
              | ExecutiveAction ea => eval_exec_action ea

    in  capability_o <- get_capability tt ot.Place cap_id;
        case capability_o of
            None => return {}
          | Some cap =>
            case deserialize cap.Action of
                RoomAction a =>
                (case a of
                     SetRuleSet => set_rule_set {}
                   | StartGame  => start_game {})
              | GameAction a =>
                (case a of
                     Vote  v => eval_vote v
                   | Timed a => run_timed a)
    end
