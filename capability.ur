open Auth
open Server_response

fun eval_capability (room_id : int)
                    (cap_id  : int)
                    (arg : option capability_arg) : transaction {} =
    pigot <- player_in_game_on_turn_exn room_id;
    let val (rt, gt, tt, ot) = (pigot.Room, pigot.Game, pigot.Turn, pigot.TableOrder)

        val current_step = current_step tt

        fun set_rule_set {} : transaction {} =
            case arg of
                Some (RuleSetArg rs) =>
                send_public_message gt (GeneralRsp (RuleSet rs))
              | _ => return {}

        fun start_game {} : transaction {} = return {}

        fun choose_chancellor (chan_id : int) : transaction {} =
            submit_chancellor tt chan_id;
            send_public_message gt (GeneralRsp (ChancellorChosen chan_id))

        fun discard (p : card) : transaction {} =
            submit_discard tt p;
            send_public_message gt (GeneralRsp PresidentDiscard)

        fun enact_policy (c : card) : transaction {} =
            let fun from_bool (b : bool) : side = if b then Liberal else Fascist

                val chosen_policy : side = from_bool (case c of
                                                          Fst => tt.Fst
                                                        | Snd => tt.Snd
                                                        | Trd => tt.Trd)
            in
                send_public_message gt (GeneralRsp (PolicyPassed chosen_policy))
            end

        fun eval_vote (v : option bool) : transaction {} =
            vote_o <- does_vote_exist_for tt ot.Place;
            (case vote_o of
                 None   =>    new_vote tt ot.Place v
               | Some _ => update_vote tt ot.Place v);
            send_public_message gt (GeneralRsp (VoteState { Place = ot.Place
                                                          , State = Option.isSome vote_o }))

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
                                                    | Some _ => is_a_liberal) place;
                            update_last_action gt
                        end
                    fun call_special_election (place : int) : transaction {} =
                        return {}
                    fun execute_player (place : int) : transaction {} =
                        kill_player tt ot.Place
                    fun president_veto {} : transaction {} =
                        president_veto_turn gt tt
                    fun reject_veto {} : transaction {} = return {}
                in  case a of
                        InvestigateLoyalty  place => investigate_loyalty   place
                      | CallSpecialElection place => call_special_election place
                      | ExecutePlayer       place => execute_player        place
                      | Veto                      => president_veto {}
                      | RejectVeto                => return {}
                end

        fun run_timed (a : timed_action) : transaction {} =
            update_last_action gt;
            case a of
                ChooseChancellor c => choose_chancellor c
              | DiscardPolicy    p => discard p
              |   EnactPolicy    p => enact_policy p
              | ExecutiveAction ea => eval_exec_action ea

    in  capability_o <- oneOrNoRows1 (SELECT *
                                      FROM capability
                                      WHERE capability.Room = {[tt.Room]}
                                        AND capability.Game = {[tt.Game]}
                                        AND capability.Turn = {[tt.Turn]}
                                        AND capability.Place = {[ot.Place]}
                                        AND capability.Capability = {[cap_id]});
        case capability_o of
            None => return {}
          | Some cap =>
            case deserialize cap.Action of
                SetRuleSet   => set_rule_set {}
              | StartGame    => start_game {}
              | Vote       v => eval_vote v
              | Timed      a => run_timed a
    end
