open Auth
open Server_response

fun eval_capability (room_id : int) (cap_id  : int) : transaction {} =
    pigot <- player_in_game_on_turn_exn room_id;
    let val (rt, gt, tt, ot) = ( pigot.Room
                               , pigot.Game
                               , pigot.Turn
                               , pigot.TableOrder )

        val current_step = current_step tt

        fun eval_vote (v : option bool) : transaction {} =
            vote_o <- does_vote_exist_for tt ot.Place;
            send_public_message gt
                                (GeneralRsp
                                     (VoteState { Place = ot.Place
                                                , State = Option.isSome vote_o }));
            case vote_o of
                None   =>    new_vote tt ot.Place v
              | Some _ => update_vote tt ot.Place v

        fun eval_exec_action (a : Types.exec_action) : transaction {} =
            if current_step <> ExecActionStep then return {}
            else
                let fun investigate_loyalty (place : int) : transaction {} =
                        submit_loyalty_investigation tt place;
                        let fun is_a_fascist (place : int) : transaction {} =
                                return {}
                            fun is_a_liberal (place : int) : transaction {} =
                                return {}
                        in  possible_liberal <- possible_liberal rt place;
                            (case possible_liberal of None   => is_a_fascist
                                                    | Some _ => is_a_liberal) place;
                            update_last_action gt
                        end
                    fun call_special_election (place : int) : transaction {} =
                        update_last_action gt
                    fun execute_player (place : int) : transaction {} =
                        if ot.Place = place then return {}
                        else kill_player gt tt ot.Place
                    fun president_veto {} : transaction {} =
                        if not tt.VetoProposed then return {}
                        else president_veto_turn gt tt
                in  previous_turn_o <- previous_turn tt;
                    let val is_new_policy =
                            case previous_turn_o of
                                None => tt.FascistPolicies = 1
                              | Some previous_turn =>
                                previous_turn.FascistPolicies < tt.FascistPolicies
                    in  (*case (tt.FascistPolicies, is_new_policy, a) of
                              (1, True,  InvestigateLoyaltyAct place) =>
                              investigate_loyalty   place
                            | (2, True, CallSpecialElectionAct place) =>
                              call_special_election place
                            | (4, True,          ExecutePlayer place) =>
                              execute_player        place
                            | (5,    _,                PresidentVeto) =>
                              president_veto {}
                            | _ =>*) return {}
                    end
                end

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
            let fun eval_action (action : Types.action) : transaction {} =
                    case action of
                        Vote v => eval_vote v
                      | ChooseChancellor c => submit_chancellor gt tt c
                      | DiscardPolicy p => submit_discard tt p
                      | EnactPolicy p => return {} (*TODO*)
                      | ExecutiveAction ea => eval_exec_action ea
            in eval_action (deserialize cap.Action)
            end
    end
