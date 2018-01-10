open Server_response
open Tables

fun game_loop (gt : game_table) =
    tt <- current_turn_state gt;
    rs <- get_rule_set tt;
    now <- now;
    let fun if_action_overdue (delta : float) (action_f : transaction {}) : transaction {} =
            if rs.TimedGame
            then
                if addSeconds gt.LastAction (ceil (delta * 60.)) < now
                then action_f
                else return {}
            else return {}

        val current_step = deserialize tt.CurrentStep

    in  if tt.LiberalPolicies = 5
        then liberals_win gt
        else
            if tt.FascistPolicies = 6
            then fascists_win gt
            else
                if current_step = ChancellorSelectStep
                then if_action_overdue rs.ChanNomTime (punish_president gt)
                else
                    if current_step = VoteStep
                    then
                        players <- players_in_game gt;
                        votes <- current_votes tt;
                        let val yes = List.filter (fn v => v.Vote = Some  True) votes
                            val no  = List.filter (fn v => v.Vote = Some False) votes
                        in  if (* TODO filter dead players *)
                                List.length (List.append yes no) <> List.length players
                            then if_action_overdue rs.GovVoteTime (punish_non_voters gt)
                            else
                                if List.length no >= List.length yes
                                then vote_failed gt
                                else
                                    reset_reject_counter tt;
                                    vote_done tt
                        end
                    else
                        (if tt.FascistPolicies <= 3
                         then return {}
                         else
                             if tt.HitlerCheckDone
                             then return {}
                             else
                                 hitler_o <- possible_hitler tt;
                                 hitler_check_done tt;
                                 case hitler_o of
                                     None   => return {}
                                   | Some _ => fascists_win gt);
                        if current_step = DiscardStep
                        then if_action_overdue rs.PresDisTime (punish_president gt)
                        else
                            if current_step = EnactStep
                            then if_action_overdue rs.ChanEnaTime (punish_chancellor gt)
                            else
                                if current_step = ExecActionStep
                                then if_action_overdue rs.ExecActTime (punish_president gt)
                                else return {}
    end

task periodic 1 = (* Loop for active games. *)
     fn {} =>
        games <- active_games {};
        mapM_ game_loop games
