open Server_response
open Tables

fun game_loop (gt : game_table) =
    tt <- current_turn_state gt;
    now <- now;
    let fun if_action_overdue (delta : float) (action_f : transaction {}) : transaction {} =
            if gt.TimedGame
            then
                if addSeconds gt.LastAction (ceil (delta * 60.)) < now
                then action_f
                else return {}
            else return {}
    in  if tt.LiberalPolicies = 5
        then liberals_win gt
        else
            if tt.FascistPolicies = 6
            then fascists_win gt
            else
                if not tt.ChancSelDone
                then if_action_overdue gt.ChanNomTime (punish_president gt)
                else
                    if not tt.VoteDone
                    then
                        players <- players_in_game gt;
                        votes <- current_votes tt;
                        let val yes = List.filter (fn v => v.Vote = Some  True) votes
                            val no  = List.filter (fn v => v.Vote = Some False) votes
                        in  if (* TODO filter dead players *)
                                List.length (List.append yes no) <> List.length players
                            then if_action_overdue gt.GovVoteTime (punish_non_voters gt)
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
                        if not tt.DiscardDone
                        then if_action_overdue gt.PresDisTime (punish_president gt)
                        else
                            if not tt.EnactionDone
                            then if_action_overdue gt.ChanEnaTime (punish_chancellor gt)
                            else
                                if not tt.ExecActionDone
                                then if_action_overdue gt.ExecActTime (punish_president gt)
                                else return {}
    end

task periodic 1 = (* Loop for active games. *)
     fn {} =>
        games <- active_games {};
        mapM_ game_loop games
