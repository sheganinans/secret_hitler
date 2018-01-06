open Auth
open Protocol
open Tables
open Utils

fun players_in_game (gt : game_table) : transaction (list player_connection) =
    queryL1 (SELECT *
             FROM player_in_game
             WHERE player_in_game.Room = {[gt.Room]}
               AND player_in_game.Game = {[gt.Game]})

fun alive_player_ordering (gt : game_table) : transaction (list table_ordering_table) =
    queryL1 (SELECT table_ordering.*
             FROM (table_ordering
                 LEFT JOIN dead_player
                 ON  table_ordering.Room  = dead_player.Room
                 AND table_ordering.Game  = dead_player.Game
                 AND table_ordering.Place = dead_player.Place)
             WHERE table_ordering.Room = {[gt.Room]}
               AND table_ordering.Game = {[gt.Game]})

fun current_turn_state (gt : game_table) : transaction turn_table =
    oneRow1 (SELECT *
             FROM turn
             WHERE turn.Room = {[gt.Room]}
               AND turn.Game = {[gt.Game]}
               AND turn.Turn = {[gt.CurrentTurn]})

fun update_last_action (gt : game_table) =
    now <- now;
    dml (UPDATE game
         SET LastAction = {[now]}
         WHERE Room = {[gt.Room]}
           AND Game = {[gt.Game]})


fun send_message_to_listeners (gt : game_table) (msg : in_game_response) : transaction {} =
    player_list <- players_in_game gt;
    mapM_ (fn p => send p.Chan msg) player_list

fun update_vote (room_id : int) (vote_b : option bool) : transaction {} =
    (* TODO send vote/unvote state to listeners *)
    pigot <- player_in_game_on_turn_exn room_id;
    let val (gt, tt, ot) = ( pigot.Game
                           , pigot.Turn
                           , pigot.TableOrder )
    in  if tt.VoteDone
        then return {}
        else vote_o <- oneOrNoRows1 (SELECT *
                                     FROM vote
                                     WHERE vote.Room  = {[tt.Room]}
                                       AND vote.Game  = {[tt.Game]}
                                       AND vote.Place = {[ot.Place]});
             send_message_to_listeners gt
                                       (GeneralRsp
                                            (VoteState
                                                 { Place = ot.Place
                                                 , State = Option.isSome vote_o }));
             case vote_o of
                 None =>
                 dml (INSERT INTO vote (Room, Game, Turn, Place, Vote)
                      VALUES ( {[tt.Room]}
                          , {[tt.Game]}
                          , {[tt.Turn]}
                          , {[ot.Place]}
                          , {[vote_b]} ))
               | Some _ =>
                 dml (UPDATE vote
                      SET Vote = {[vote_b]}
                      WHERE Room  = {[tt.Room]}
                        AND Game  = {[tt.Game]}
                        AND Turn  = {[tt.Turn]}
                        AND Place = {[ot.Place]})
    end


fun eval_president_action (room_id : int) (a : Protocol.president) : transaction {} =
    pigot <- player_in_game_on_turn_exn room_id;
    let val (gt, tt, ot) = ( pigot.Game
                           , pigot.Turn
                           , pigot.TableOrder )
        fun choose_chancellor (chan_id : int) : transaction {} =
            if tt.ChancSelDone then return {}
            else
                dml (UPDATE turn
                     SET Chancellor = {[chan_id]}
                     WHERE Room = {[tt.Room]}
                       AND Game = {[tt.Game]}
                       AND Turn = {[tt.Turn]});
                dml (UPDATE turn
                     SET ChancSelDone = TRUE
                     WHERE Room = {[tt.Room]}
                       AND Game = {[tt.Game]}
                       AND Turn = {[tt.Turn]});
                update_last_action gt;
                send_message_to_listeners gt (GeneralRsp (ChancellorChosen chan_id))
        fun discard_card (card_id : int) : transaction {} =
            if tt.DiscardDone then return {}
            else
                if card_id > 3 || card_id <= 0
                then return {}
                else
                    dml (UPDATE turn
                         SET PresDisc = {[card_id]}
                         WHERE Room = {[tt.Room]}
                           AND Game = {[tt.Game]}
                           AND Turn = {[tt.Turn]});
                    dml (UPDATE turn
                         SET DiscardDone = TRUE
                         WHERE Room = {[tt.Room]}
                           AND Game = {[tt.Game]}
                           AND Turn = {[tt.Turn]});
                    update_last_action gt;
                    send_message_to_listeners gt (GeneralRsp PresidentDiscard)

        fun if_all_steps_up_to_done (s : step) : transaction {} = return {}

        fun eval_exec_action (a : Protocol.executive_action) : transaction {} =
            if tt.ExecActionDone
               (*|| not (tt.ChancSelDone && tt.VoteDone && )*)
            then return {}
            else
                previous_turn_o <- oneOrNoRows1 (SELECT *
                                                 FROM turn
                                                 WHERE turn.Room = {[tt.Room]}
                                                   AND turn.Game = {[tt.Game]}
                                                   AND turn.Turn = {[tt.Turn - 1]});
                case previous_turn_o of
                    None => return {}
                  | Some previous_turn =>
                    case ( tt.FascistPolicies
                         , previous_turn.FascistPolicies < tt.FascistPolicies
                         , a ) of
                        (1, True,  InvestigateLoyaltyAct place) => investigate_loyalty   place
                      | (2, True, CallSpecialElectionAct place) => call_special_election place
                      | (4, True,          ExecutePlayer place) => execute_player        place
                      | (5,    _,                PresidentVeto) => president_veto {}
                      | _ => return {}

        and investigate_loyalty (place : int) : transaction {} =
            dml (INSERT INTO loyalty_investigation (Room, Game, Turn, Place)
                 VALUES ({[tt.Room]}, {[tt.Game]}, {[tt.Turn]}, {[place]}));
            let fun is_a_fascist (place : int) : transaction {} = return {}
                fun is_a_liberal (place : int) : transaction {} = return {}
            in  possible_liberal <- oneOrNoRows1 (SELECT *
                                                  FROM liberal
                                                  WHERE liberal.Room = {[tt.Room]}
                                                    AND liberal.Game = {[tt.Game]}
                                                    AND liberal.Place = {[place]});
                (case possible_liberal of
                    None => is_a_fascist place
                  | Some _  => is_a_liberal place);
                update_last_action gt
            end
        and call_special_election (place : int) : transaction {} = update_last_action gt
        and execute_player (place : int) : transaction {} =
            if ot.Place = place
            then return {}
            else
                dml (INSERT INTO dead_player (Room, Game, Turn, Place)
                     VALUES ({[tt.Room]}, {[tt.Game]}, {[tt.Turn]}, {[place]}));
                send_message_to_listeners gt (GeneralRsp (PlayerExecuted place));
                update_last_action gt
        and president_veto {} : transaction {} =
            if not tt.VetoProposed
            then return {}
            else
                dml (INSERT INTO veto (Room, Game, Turn, President, Chancellor)
                     VALUES ({[tt.Room]}
                         , {[tt.Game]}
                         , {[tt.Turn]}
                         , {[tt.President]}
                         , {[tt.Chancellor]}));
                send_message_to_listeners gt (GeneralRsp VetoEnacted);
                update_last_action gt
    in  if ot.Place <> tt.President then return {}
        else
            case a of
                StandardAction a =>
                (case a of
                     ChooseChancellor     chan_id => choose_chancellor chan_id
                   | PresidentDiscardCard card_id =>      discard_card card_id)
              | ExecutiveActionMsg a => eval_exec_action a

    end

fun player_action (room_id : int) (action : Protocol.in_game) : transaction {} =
    case action of
      VoterAction vote_b => update_vote room_id vote_b
    | PresidentAction a => eval_president_action room_id a
    | ChancellorAction a =>
      pigot <- player_in_game_on_turn_exn room_id;
      let val tt = pigot.Turn
      in  case a of
              EnactPolicy _ => return {}
            | ProposeVeto =>
              case tt.FascistPolicies of
                  5 => return {}
                | _ => return {}
      end
    | ChatAction a => return {}
