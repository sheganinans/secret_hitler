open Player_action

fun skip_turn (gt : game_table) : transaction {} =
    current_turn <- current_turn_state gt;
    deck <- next_turn_deck_state { LiberalsInDraw = current_turn.LiberalsInDraw
                                 , FascistsInDraw = current_turn.FascistsInDraw
                                 , LiberalsInDisc = current_turn.LiberalsInDisc
                                 , FascistsInDisc = current_turn.FascistsInDisc };
    dml (INSERT INTO turn
           ( Room
             , Game
             , Turn
             , President
             , NextPres
             , Chancellor
             , RejectCount
             , VetoProposed
             ,    ChancSelDone
             ,        VoteDone
             , HitlerCheckDone
             ,     DiscardDone
             ,    EnactionDone
             ,  ExecActionDone
             , LiberalsInDraw
             , FascistsInDraw
             , LiberalsInDisc
             , FascistsInDisc
             , Fst
             , Snd
             , Trd
             , PresDisc
             , ChanEnac
             , LiberalPolicies
             , FascistPolicies )
         VALUES
           ( {[current_turn.Room]}
             , {[current_turn.Game]}
             , {[current_turn.Turn + 1]}
             , {[current_turn.NextPres]}
             , {[current_turn.NextPres + 1]} (* TODO add rollover and skipping dead *)
             , 0
             , {[current_turn.RejectCount + 1]}
             , FALSE
             , FALSE
             , FALSE
             , FALSE
             , FALSE
             , FALSE
             , FALSE
             , {[deck.LibDraw]}
             , {[deck.FasDraw]}
             , {[deck.LibDisc]}
             , {[deck.FasDisc]}
             , {[deck.Fst]}
             , {[deck.Snd]}
             , {[deck.Trd]}
             , 0
             , 0
             , {[current_turn.LiberalPolicies]}
             , {[current_turn.FascistPolicies]} ))

fun enact_skip_turn_or_kill (gt : game_table)
                            (kill_f : turn_table -> transaction {}) : transaction {} =
    if gt.KillPlayer
    then turn <- current_turn_state gt; kill_f turn
    else skip_turn gt

fun send_punished_list (gt : game_table) (l : list int) : transaction {} =
    send_message_to_listeners gt (GeneralRsp (PlayersPunished l))

fun punish_president (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn turn =>
        dml (INSERT INTO dead_player (Room, Game, Turn, Place)
             VALUES ( {[turn.Room]}
                 , {[turn.Game]}
                 , {[turn.Turn]}
                 , {[turn.President]} ));
        send_punished_list gt (turn.President :: []))

fun punish_chancellor (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn turn =>
        dml (INSERT INTO dead_player (Room, Game, Turn, Place)
             VALUES ( {[turn.Room]}
                 , {[turn.Game]}
                 , {[turn.Turn]}
                 , {[turn.Chancellor]} ));
        send_punished_list gt (turn.Chancellor :: []))

fun punish_non_voters (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn turn =>
        non_voters <- queryL1 (SELECT table_ordering.Place
                               FROM (table_ordering
                                   LEFT JOIN vote
                                   ON  table_ordering.Room = {[gt.Room]}
                                   AND table_ordering.Game = {[gt.Game]}
                                   AND table_ordering.Place = vote.Place)
                               WHERE table_ordering.Room = {[gt.Room]}
                                 AND table_ordering.Game = {[gt.Game]});
        mapM_ (fn p =>
                  dml (INSERT INTO dead_player (Room, Game, Turn, Place)
                       VALUES ( {[turn.Room]}
                           , {[turn.Game]}
                           , {[turn.Turn]}
                           , {[p.Place]} )))
              non_voters;
        send_punished_list gt (List.mp (fn n => n.Place) non_voters))

fun govt_in_chaos (gt : game_table) : transaction {} = return {}

fun vote_failed (gt : game_table) : transaction {} =
    tt <- current_turn_state gt;
    dml (UPDATE turn
         SET RejectCount = {[tt.RejectCount + 1]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]});
    let val state = if tt.RejectCount = 2 then InChaos else Failed
    in  send_message_to_listeners gt (GeneralRsp (NewGovt state));
        (case state of
             InChaos => govt_in_chaos gt
           | Failed  =>     skip_turn gt
           | Passed => return {(* Will never happen *)})
    end

fun gen_game_end_state (gt : game_table)
                       (winners : side) : transaction (option game_end_state) =
    case gt.GameEnded of
        None => return None
      | Some end_time =>
        hitler <- oneRow1 (SELECT *
                           FROM hitler
                           WHERE hitler.Room = {[gt.Room]}
                             AND hitler.Game = {[gt.Game]});
        fascists <- queryL1 (SELECT *
                             FROM fascist
                             WHERE fascist.Room = {[gt.Room]}
                               AND fascist.Game = {[gt.Game]});
        liberals <- queryL1 (SELECT *
                             FROM liberal
                             WHERE liberal.Room = {[gt.Room]}
                               AND liberal.Game = {[gt.Game]});
        dead <- queryL1 (SELECT *
                         FROM dead_player
                         WHERE dead_player.Room = {[gt.Room]}
                           AND dead_player.Game = {[gt.Game]});

        return <| Some
               { Winners = winners
               , Hitler = hitler.Place
               , Liberals = List.mp (fn l => l.Place) liberals
               , Fascists = List.mp (fn f => f.Place) fascists
               , Dead = List.mp (fn d => { Turn = d.Turn, Place = d.Place }) dead
               , Start = gt.GameStarted
               , End = end_time
               }

fun winners (gt : game_table) (winners : side) : transaction {} =
    game_end_state_o <- gen_game_end_state gt Liberal;
    case game_end_state_o of
        None => return {}
      | Some game_end_state =>
        send_message_to_listeners gt (GeneralRsp (GameEndState game_end_state))

fun liberals_win (gt : game_table) : transaction {} = winners gt Liberal

fun fascists_win (gt : game_table) : transaction {} = winners gt Fascist

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
                        votes <- queryL1 (SELECT *
                                          FROM vote
                                          WHERE vote.Room = {[gt.Room]}
                                            AND vote.Game = {[gt.Game]}
                                            AND vote.Turn = {[tt.Turn]});
                        let val yes = List.filter (fn v => v.Vote = Some  True) votes
                            val no  = List.filter (fn v => v.Vote = Some False) votes
                        in  if (* TODO filter dead players *)
                                List.length (List.append yes no) <> List.length players
                            then if_action_overdue gt.GovVoteTime (punish_non_voters gt)
                            else
                                if List.length no >= List.length yes
                                then vote_failed gt
                                else
                                    dml (UPDATE turn
                                         SET RejectCount = 0
                                         WHERE Room = {[tt.Room]}
                                           AND Game = {[tt.Game]}
                                           AND Turn = {[tt.Turn]});
                                    dml (UPDATE turn
                                         SET VoteDone = TRUE
                                         WHERE Room = {[tt.Room]}
                                           AND Game = {[tt.Game]}
                                           AND Turn = {[tt.Turn]})
                        end
                    else
                        (if tt.FascistPolicies <= 3
                         then return {}
                         else
                             if tt.HitlerCheckDone
                             then return {}
                             else
                                 hitler_o <- oneOrNoRows1 (SELECT *
                                                           FROM hitler
                                                           WHERE hitler.Room = {[gt.Room]}
                                                             AND hitler.Game = {[gt.Game]}
                                                             AND hitler.Place =
                                                             {[tt.Chancellor]});
                                 dml (UPDATE turn
                                      SET HitlerCheckDone = TRUE
                                      WHERE Room = {[tt.Room]}
                                        AND Game = {[tt.Game]}
                                        AND Turn = {[tt.Turn]});
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
        games <- queryL1 (SELECT game.*
                          FROM (game
                              INNER JOIN room
                              ON  game.Room = room.Room
                              AND game.Game = room.CurrentGame));
        mapM_ game_loop games
