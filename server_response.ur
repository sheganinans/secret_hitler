open Protocol
open Tables
open Types
open Utils

fun govt_in_chaos (gt : game_table) : transaction {} = return {}

fun vote_failed (gt : game_table) : transaction {} =
    tt <- current_turn_state gt;
    incr_reject_counter tt;
    let val state = if tt.RejectCount = 2 then InChaos else Failed
    in  send_public_message gt (NewGovt state);
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
        hitler   <- get_hitler   gt;
        fascists <- get_fascists gt;
        liberals <- get_liberals gt;
        dead     <- get_dead     gt;

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
    game_end_state_o <- gen_game_end_state gt winners;
    case game_end_state_o of
        None => return {}
      | Some game_end_state =>
        no_longer_in_game gt;
        send_public_message gt (GameEndState game_end_state)

fun liberals_win (gt : game_table) : transaction {} = winners gt Liberal

fun fascists_win (gt : game_table) : transaction {} = winners gt Fascist

fun enact_skip_turn_or_kill (gt : game_table)
                            (kill_f : turn_table -> transaction {}) : transaction {} =
    rs <- get_rule_set gt;
    if rs.KillPlayer
    then turn <- current_turn_state gt; kill_f turn
    else skip_turn gt

fun send_punished_list (gt : game_table) (l : list int) : transaction {} =
    send_public_message gt (PlayersPunished l);
    hitler <- get_hitler gt;
    if List.exists (fn p => p = hitler.Place) l
    then liberals_win gt
    else return {}

fun punish_president (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn tt =>
        kill_player tt tt.President;
        send_punished_list gt (tt.President :: []))

fun punish_chancellor (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn tt =>
        kill_player tt tt.Chancellor;
        send_punished_list gt (tt.Chancellor :: []))

fun punish_non_voters (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn tt =>
        non_voters <- queryL1 (SELECT table_ordering.Place
                               FROM (table_ordering
                                   LEFT JOIN vote
                                   ON  table_ordering.Room = {[gt.Room]}
                                   AND table_ordering.Game = {[gt.Game]}
                                   AND table_ordering.Place = vote.Place)
                               WHERE table_ordering.Room = {[gt.Room]}
                                 AND table_ordering.Game = {[gt.Game]});
        mapM_ (fn p => kill_player tt p.Place) non_voters;
        send_punished_list gt (List.mp (fn n => n.Place) non_voters))
