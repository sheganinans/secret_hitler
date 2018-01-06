open Auth
open Protocol
open Tables
open Utils

fun signup_page {} : transaction page =
    let fun submit_signup (signup : player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_hash signup.PassHash
            in  u_o <- oneOrNoRows1 (SELECT player.Username
                                     FROM player
                                     WHERE player.Username = {[signup.Username]});
                case u_o of
                    Some u => login_page {}
                  | None =>
                    player_id <- nextval player_seq;
                    dml (INSERT INTO player (Player, Username, PassHash)
                         VALUES
                           ( {[player_id]}
                             , {[signup.Username]}
                             , {[pw_hs]} ));
                    set_username_cookie signup.Username pw_hs;
                    main_menu {}
            end
    in  return <xml><body><form>
      <table>
        <tr><th>Signup</th></tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_signup}/></td></tr>
      </table></form></body></xml>
    end

and login_page {} : transaction page = return <xml><body>{login_form {}}</body></xml>

and login_form {} : make_form = <xml><table>
  <tr><td><a link={main_menu {}}>Back to Main Menu</a></td></tr>
  <tr><td><a link={signup_page {}}>Need to signup?</a></td></tr>
  <tr><td>
    <form>
      <table>
        <tr>Login!</tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_login}/></td></tr>
       </table></form></td></tr>
  </table></xml>

and submit_login (login_form : player_name_and_pass) : transaction page =
    let val pw_hs = Auth.basic_hash login_form.PassHash
    in  hp_o <- oneOrNoRows1 (SELECT player.PassHash
                              FROM player
                              WHERE player.Username = {[login_form.Username]});
        case hp_o of
            None => login_page {}
          | Some hp =>
            if hp.PassHash <> pw_hs
            then login_page {}
            else set_username_cookie login_form.Username pw_hs;
                 main_menu {}
    end

and kick_player (room_id : int) : transaction page = return <xml></xml>

and kicked_body (rt : room_table) : xbody =
    <xml><table>
      <tr><th>You have been kicked from room: {[rt.Nam]}</th></tr>
      <tr>Appeal system coming soon!</tr>
      <tr><td><a link={main_menu {}}>Main Menu</a></td></tr>
    </table></xml>

and kicked (room_id : int) : transaction page =
    (_, rt) <- player_in_room_exn room_id;
    return <xml><body>{kicked_body rt}</body></xml>

and main_menu {} : transaction page =
    pt <- check_role Player;
    return <xml><body>{[main_menu_body ()]}</body></xml>

and main_menu_body {} : xbody = <xml>
  <table>
    <tr><td><a link={new_room {}}>New Room</a></td></tr>
    <tr><td><a link={new_game {}}>New Game</a></td></tr>
    <tr><td><a link={view_your_rooms {}}>View Your Rooms</a></td></tr>
    <tr><td><a link={view_public_rooms {}}>View Public Rooms</a></td></tr>
  </table></xml>

and new_room {} : transaction page =
    let fun submit_new_room room_form : transaction page =
            pt      <- check_role Player;
            now     <- now;
            room_id <- nextval room_seq;
            pass    <- (if room_form.Private
                        then rand <- rand; return (Some (basic_hash (show rand)))
                        else               return None);
            dml (INSERT INTO room (Room, OwnedBy, Nam, Pass, CurrentGame, InGame)
                 VALUES
                   ( {[room_id]}
                     , {[pt.Player]}
                     , {[room_form.RoomName]}
                     , {[pass]}
                     , 0
                     , FALSE ));
            dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                 VALUES ({[room_id]}, {[pt.Player]}, {[pt.Player]}, {[now]}));
            view_room room_id

    in  return <xml><body><form>
          <table>
            <tr><th>New Room</th></tr>
            <tr><th>Name</th><td><textbox{#RoomName}/></td></tr>
            <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
            <tr><td><submit action={submit_new_room}/></td></tr>
          </table></form></body></xml>
    end

and new_game {} : transaction page =
    let fun submit_new_game (room_id: int) (rule_set : rule_set) : transaction page =
            (pt, rt) <- only_if_owner_mod_exn room_id;
            r_o <- oneOrNoRows1 (SELECT *
                                 FROM game
                                 WHERE game.Room = {[rt.Room]}
                                   AND game.Game = {[rt.CurrentGame]});
            (case r_o of
                Some _ => return {}
              | None   =>
                now <- now;
                dml (INSERT INTO game
                       ( Room
                         , Game
                         , TimedGame
                         , KillPlayer
                         , ChanNomTime
                         , GovVoteTime
                         , PresDisTime
                         , ChanEnaTime
                         , ExecActTime
                         , CurrentTurn
                         , GameStarted
                         , LastAction
                         , GameEnded )
                     VALUES
                       ( {[rt.Room]}
                         , {[rt.CurrentGame]}
                         , {[rule_set.TimedGame]}
                         , {[rule_set.KillPlayer]}
                         , {[rule_set.ChanNomTime]}
                         , {[rule_set.GovVoteTime]}
                         , {[rule_set.PresDisTime]}
                         , {[rule_set.ChanEnaTime]}
                         , {[rule_set.ExecActTime]}
                         , 0
                         , {[now]}
                         , {[now]}
                         , {[None]} )));
            view_room rt.Room

        fun new_game_page (room_id : int) {} : transaction page =
            return <xml><body><form>
              <table>
                <tr><th>New Game: Time Table</th></tr>
                <tr><th>Timed Game?</th>
                  <td><checkbox{#TimedGame}/></td></tr>
                <tr><th>Kill Player as Punishment?</th>
                  <td><checkbox{#KillPlayer}/></td></tr>
                <tr><th>Chancellor Nomination</th>
                  <td><number{#ChanNomTime}/></td></tr>
                <tr><th>Government Vote</th>
                  <td><number{#GovVoteTime}/></td></tr>
                <tr><th>President Discard</th>
                  <td><number{#PresDisTime}/></td></tr>
                <tr><th>Chancellor Enaction</th>
                  <td><number{#ChanEnaTime}/></td></tr>
                <tr><th>Executive Action</th>
                  <td><number{#ExecActTime}/></td></tr>
                <tr><td><submit action={submit_new_game room_id}/></td></tr>
              </table></form></body></xml>

    in  room_list <- select_rooms_controlled {};
        return <xml><body>
          {List.mapX (fn r =>
                         <xml><form><submit value={r.Nam}
                                            action={new_game_page r.Room}/>
                         </form></xml>)
                     room_list}
        </body></xml>
    end

and view_your_rooms {} : transaction page =
    rl <- select_rooms_controlled {};
    case rl of
        [] => main_menu {}
      | rl =>
        return <xml><body>
          <table>
            {List.mapX (fn r =>
                           <xml><tr><td>
                             <a link={view_room r.Room}>
                               {[r.Nam]}</a></td></tr></xml>)
                       rl}
          </table></body></xml>

and view_public_rooms {} : transaction page =
    rl <- queryL1 (SELECT * FROM room WHERE room.Pass = NULL);
    return <xml><body><table>
      <tr><td><a link={main_menu {}}>Main Menu</a></td></tr>
      {List.mapX (fn r =>
                     <xml><tr><td>
                       <a link={join_room r.Room}>
                         {[show r.Nam]}</a></td></tr></xml>)
                 rl}
    </table></body></xml>

and view_invite (room_id : int) : transaction page =
    (pt, rt) <- player_in_room_exn room_id;
    return <xml><body><table>
      <tr><th><a link={view_room room_id}>Back to {[rt.Nam]}</a></th></tr>
      <tr><th>Link:</th><td>{[show (url (view_room room_id))]}</td></tr>
      {[case rt.Pass of
            None => <xml></xml> : xtable
          | Some pass => <xml><tr><th>Pass:</th><td>{[pass]}</td></tr></xml>]}
    </table></body></xml>

and join_room (room_id : int) : transaction page =
    let fun submit_private_room_secret (room_id : int)
                                       (pass : { Pass : string }) : transaction page =
            rt <- room_exists_exn room_id;
            (if rt.Pass = None || rt.Pass = Some pass.Pass
             then pt  <- check_role Player;
                  now <- now;
                  dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                       VALUES ({[room_id]}, {[pt.Player]}, {[pt.Player]}, {[now]}))
             else return {});
            view_room room_id

    in  pt <- check_role Player;
        rp_o <- oneOrNoRows1 (SELECT *
                              FROM room_player
                              WHERE room_player.Player = {[pt.Player]}
                                AND room_player.Room = {[room_id]});
        case rp_o of
            Some _ => view_room room_id
          | None   =>
            rt <- room_exists_exn room_id;
            case rt.Pass of
                None =>
                now <- now;
                dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                     VALUES ({[room_id]}, {[pt.Player]}, {[pt.Player]}, {[now]}));
                view_room room_id
              | Some _ =>
                return <xml><body><form><table>
                  <tr><th>Pass</th><td><password{#Pass}/></td></tr>
                  <tr><td><submit action={submit_private_room_secret room_id}/></td></tr>
                </table></form></body></xml>
    end

and join_game (room_id : int) : transaction page =
    let fun submit_join_game (room_id : int)
                             (playing : { Playing : bool }) : transaction page =
            (pt, rt) <- player_in_room_exn room_id;
            gt_o <- oneOrNoRows1 (SELECT *
                                  FROM game
                                  WHERE game.Game = {[rt.CurrentGame]});
            (case gt_o of
                None => return {}
              | Some gt =>
                me <- self;
                chan <- channel;
                num_in_game <- oneRowE1 (SELECT COUNT ( * )
                                         FROM player_in_game
                                         WHERE player_in_game.Room = {[rt.Room]}
                                           AND player_in_game.Game = {[rt.CurrentGame]});
                dml (INSERT INTO player_in_game
                       ( Room
                         , Game
                         , Player
                         , Client
                         , Chan
                         , Watching
                         , InGameId )
                     VALUES
                       ( {[rt.Room]}
                         , {[rt.CurrentGame]}
                         , {[pt.Player]}
                         , {[me]}
                         , {[chan]}
                         , {[if rt.InGame then True else not playing.Playing]}
                         , {[num_in_game + 1]} )));
            view_room room_id

    in  (pt, rt) <- player_in_room_exn room_id;
        if rt.InGame
        then submit_join_game room_id {Playing = False}
        else return <xml><body><form><table>
                      <tr><th>Playing?</th><td><checkbox{#Playing}/></td></tr>
                      <tr><submit action={submit_join_game room_id}/></tr>
                    </table></form></body></xml>
    end

and chat (chat_id : int) : transaction xbody =
    return <xml></xml>

and view_room (room_id : int) : transaction page =
    (pt, rt) <- only_if_player_not_kicked_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Player = {[pt.Player]}
                            AND room_player.Room   = {[rt.Room]});
    case rp_o of
        None   => join_room room_id
      | Some _ =>
        (gs : source private_game_state)
            <- source { PublicGameState =
                        { CurrentTurn =
                          { President       = 0
                          , Chancellor      = 0
                          , FascistPolicies = 0
                          , LiberalPolicies = 0
                          }
                        , GameHistory = []
                        , ChatHistory = []
                        , Players     = []
                        }
                      , GameRole          = Watcher
                      , KnownAffiliations = []
                      , Top3CardsInDraw   = []
                      };
        let fun no_game_yet {} : transaction xbody = return <xml></xml>

            fun join_game_view {} : transaction xbody = return <xml></xml>

            fun fascist_view {} : transaction xbody = return <xml></xml>

            fun hitler_view {} : transaction xbody = return <xml></xml>

            fun liberal_view {} : transaction xbody = return <xml></xml>

            fun watcher_view {} : transaction xbody = return <xml></xml>

        in  player_list <- get_all_un_and_id_in_room room_id;
            return <xml><body onload={return {}}><table>
              <tr><th>View Room {[show rt.Nam]}</th></tr>
              {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                         player_list}
            </table></body></xml>
        end

and start_game (room_id : int) : transaction page =
    (pt, rt) <- only_if_owner_mod_exn room_id;
    player_list <- queryL1 (SELECT *
                            FROM player_in_game
                            WHERE player_in_game.Room = {[rt.Room]}
                              AND player_in_game.Game = {[rt.CurrentGame]});
    let val in_game_players = List.filter (fn p => not p.Watching) player_list
        val in_game_players_l = List.length in_game_players
        val get_pid = get_player_from_in_game_id rt
    in  (case List.find (fn (k,_) => k = in_game_players_l) player_numbers_table of
             None    => return {}
           | Some (_, lf) =>
             in_game_players <- Utils.shuffle in_game_players;

             mapM_ (fn (i,p) => dml (INSERT INTO table_ordering
                                       (Room, Game, InGameId, Place)
                                     VALUES
                                       ( {[rt.Room]}
                                         , {[rt.CurrentGame]}
                                         , {[p.InGameId]}
                                         , {[i]} )))
                   (List.mapi (fn i p => (i+1,p)) in_game_players);

             in_ordering_players <- queryL1 (SELECT *
                                             FROM table_ordering
                                             WHERE table_ordering.Room = {[rt.Room]}
                                               AND table_ordering.Game = {[rt.CurrentGame]});

             in_ordering_players <- Utils.shuffle in_ordering_players;

             mapM_ (fn i =>
                       dml (INSERT INTO liberal (Room, Game, Place)
                            VALUES
                              ( {[rt.Room]}
                                , {[rt.CurrentGame]}
                                , {[i.Place]} )))
                   (List.take lf.Liberals in_ordering_players);

             dml (INSERT INTO hitler (Room, Game, Place)
                  VALUES
                    ( {[rt.Room]}
                      , {[rt.CurrentGame]}
                      , {[List.nth in_ordering_players lf.Liberals
                            |> Option.unsafeGet
                            |> fn p => p.Place]} ));

             mapM_ (fn i =>
                       dml (INSERT INTO fascist (Room, Game, Place)
                            VALUES
                              ( {[rt.Room]}
                                , {[rt.CurrentGame]}
                                , {[i.Place]} )))
                   (List.drop (lf.Liberals + 1) in_ordering_players);

             deck <- next_turn_deck_state { LiberalsInDraw = number_of_liberal_policies
                                          , FascistsInDraw = number_of_fascist_policies
                                          , LiberalsInDisc = 0
                                          , FascistsInDisc = 0 };
             dml (INSERT INTO turn
                    ( Room
                      , Game
                      , Turn
                      , President
                      , NextPres
                      , Chancellor
                      , RejectCount
                      , VetoProposed
                      , ChancSelDone
                      , VoteDone
                      , DiscardDone
                      , EnactionDone
                      , ExecActionDone
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
                    ( {[rt.Room]}
                      , {[rt.CurrentGame]}
                      , 1
                      , 1
                      , 2
                      , 0
                      , 0
                      , FALSE
                      , FALSE
                      , FALSE
                      , FALSE
                      , FALSE
                      , FALSE
                      , {[deck.LibDraw]}
                      , {[deck.FasDraw]}
                      , 0
                      , 0
                      , {[deck.Fst]}
                      , {[deck.Snd]}
                      , {[deck.Trd]}
                      , 0
                      , 0
                      , 0
                      , 0 ));

             dml (UPDATE room
                  SET InGame = TRUE
                  WHERE Room = {[rt.Room]});

             now <- now;
             dml (UPDATE game
                  SET LastAction = {[now]}
                  WHERE Room = {[rt.Room]}
                    AND Game = {[rt.CurrentGame]});

             dml (UPDATE game
                  SET CurrentTurn = 1
                  WHERE Room = {[rt.Room]}
                    AND Game = {[rt.CurrentGame]}));
        view_room room_id
    end

and get_all_un_and_id_in_room (room_id : int) : transaction (list player_id_and_username) =
    _ <- player_in_room_exn room_id;
    queryL1 (SELECT player.Player, player.Username
             FROM (room_player
                 INNER JOIN player
                 ON room_player.Player = player.Player)
             WHERE room_player.Room = {[room_id]})

and new_mod {} : transaction page =
    let fun submit_new_mod (room_id : int) (player_id : int) {} =
            (pt, rt) <- only_if_owner_mod_exn room_id;
            now <- now;
            dml (INSERT INTO mod (Room, Player, SetBy, Time)
                 VALUES ({[rt.Room]}, {[player_id]}, {[pt.Player]}, {[now]}));
            view_room room_id

        fun new_mod_page (room_id : int) {} : transaction page =
            player_list <- get_all_un_and_id_in_room room_id;
            return <xml><body>
              {List.mapX (fn p =>
                             <xml><form>
                               <submit value={p.Username}
                                       action={submit_new_mod room_id p.Player}/>
                             </form></xml>)
                         player_list}</body></xml>

    in  room_list <- select_rooms_controlled {};
        return <xml><body>
          {List.mapX (fn r =>
                         <xml><form><submit value={r.Nam}
                                            action={new_mod_page r.Room}/>
                         </form></xml>)
                     room_list}
        </body></xml>
    end

and rem_mod {} : transaction page = return <xml></xml>

and check_role (role : role) : transaction player_table =
    check <- Auth.check_login role;
    case check of
        Err (_ : string) => error <xml>{login_form {}}</xml>
      | Ok   pt          => return pt

and room_exists_exn (room_id : int) : transaction room_table =
    rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case rt_o of
        None => error <xml>{main_menu_body {}}</xml>
      | Some rt => return rt

and player_in_room_exn (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Room   = {[rt.Room]}
                            AND room_player.Player = {[pt.Player]});
    case rp_o of
        None => error <xml>{main_menu_body ()}</xml>
      | Some _ => return (pt, rt)

and player_in_game_on_turn_exn (room_id : int)
    : transaction { Player     :         player_table
                  , Room       :           room_table
                  , Game       :           game_table
                  , Conn       : player_connection
                  , TableOrder : table_ordering_table
                  , Turn       :           turn_table
                  } =
    (pt, rt) <- player_in_room_exn room_id;
    gt <- oneRow1 (SELECT *
                   FROM game
                   WHERE game.Room = {[rt.Room]}
                     AND game.Game = {[rt.CurrentGame]});
    pc <- oneRow1 (SELECT *
                    FROM player_in_game
                    WHERE player_in_game.Room = {[rt.Room]}
                      AND player_in_game.Game = {[rt.CurrentGame]});
    ot <- oneRow1 (SELECT *
                   FROM table_ordering
                   WHERE table_ordering.Room = {[rt.Room]}
                     AND table_ordering.Game = {[rt.CurrentGame]}
                     AND table_ordering.InGameId = {[pc.InGameId]});
    tt <- oneRow1 (SELECT *
                   FROM turn
                   WHERE turn.Room = {[rt.Room]}
                     AND turn.Game = {[rt.CurrentGame]}
                     AND turn.Turn = {[gt.CurrentTurn]});
    return { Player = pt, Room = rt, Game =  gt, Conn = pc, TableOrder =  ot, Turn = tt }

and only_if_owner_mod_exn (room_id : int) : transaction (player_table * room_table) =
    (pt, rt) <- player_in_room_exn room_id;
    m <- oneOrNoRows1 (SELECT *
                       FROM mod
                       WHERE mod.Room   = {[room_id]}
                         AND mod.Player = {[pt.Player]});
    if rt.OwnedBy = pt.Player || m <> None
    then return (pt, rt)
    else error <xml>{main_menu_body {}}</xml>

and only_if_player_not_kicked_exn (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    is_kicked <- oneOrNoRows1 (SELECT *
                               FROM kick
                               WHERE kick.Player = {[pt.Player]}
                                 AND kick.Room = {[rt.Room]});
    case is_kicked of
        Some _ => error <xml>{kicked_body rt}</xml>
      | None => return (pt, rt)

and if_player_in_good_standing (room_id : int)
                               (page_f : player_table * room_table -> transaction page)
    : transaction page =
    (pt, rt) <- only_if_player_not_kicked_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Player = {[pt.Player]}
                            AND room_player.Room   = {[rt.Room]});
    case rp_o of
        None   => join_room room_id
      | Some _ => page_f (pt, rt)

and select_rooms_controlled {} : transaction (list room_table) =
    pt <- check_role Player;
    rl_1 <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
    rl_2 <- queryL1 (SELECT room.*
                     FROM (room
                         INNER JOIN mod
                         ON room.Room   = mod.Room
                         AND mod.Player = {[pt.Player]}));
    return (List.append rl_1 rl_2)

fun players_in_game (gt : game_table) : transaction (list player_connection) =
    queryL1 (SELECT *
             FROM player_in_game
             WHERE player_in_game.Room = {[gt.Room]}
               AND player_in_game.Game = {[gt.Game]})

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
    let val (tt, ot) = ( pigot.Turn
                       , pigot.TableOrder )
    in  if tt.VoteDone
        then return {}
        else vote_o <- oneOrNoRows1 (SELECT *
                                     FROM vote
                                     WHERE vote.Room  = {[tt.Room]}
                                       AND vote.Game  = {[tt.Game]}
                                       AND vote.Place = {[ot.Place]});
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

        fun investigate_loyalty (place : int) : transaction {} =
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
        fun call_special_election (place : int) : transaction {} = update_last_action gt
        fun execute_player (place : int) : transaction {} =
            dml (INSERT INTO dead_player (Room, Game, Turn, Place)
                 VALUES ({[tt.Room]}, {[tt.Game]}, {[tt.Turn]}, {[place]}));
            update_last_action gt
        fun president_veto {} : transaction {} =
            if not tt.VetoProposed
            then return {}
            else
                dml (INSERT INTO veto (Room, Game, Turn, President, Chancellor)
                     VALUES ({[tt.Room]}
                         , {[tt.Game]}
                         , {[tt.Turn]}
                         , {[tt.President]}
                         , {[tt.Chancellor]}));
                update_last_action gt
    in  if ot.Place <> tt.President then return {}
        else
            case a of
                StandardAction a =>
                (case a of
                     ChooseChancellor     chan_id => choose_chancellor chan_id
                   | PresidentDiscardCard card_id =>      discard_card card_id)
              | ExecutiveActionMsg a =>
                if tt.ExecActionDone then return {}
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
             ,   ChancSelDone
             ,       VoteDone
             ,    DiscardDone
             ,   EnactionDone
             , ExecActionDone
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
             , {[deck.LibDraw]}
             , {[deck.FasDraw]}
             , {[deck.LibDisc]}
             , {[deck.LibDisc]}
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

fun gen_game_end_state (gt : game_table) (side : side) : transaction game_end_state =
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

    now <- now;
    return { Winners = side
           , Hitler = hitler.Place
           , Liberals = List.mp (fn l => l.Place) liberals
           , Fascists = List.mp (fn f => f.Place) fascists
           , Dead = List.mp (fn d => { Turn = d.Turn, Place = d.Place }) dead
           , Start = gt.GameStarted
           , End = Option.unsafeGet gt.GameEnded }

fun liberals_win (gt : game_table) : transaction {} =
    game_end_state <- gen_game_end_state gt Liberal;
    send_message_to_listeners gt (GeneralRsp (GameEndState game_end_state))

fun fascists_win (gt : game_table) : transaction {} =
    game_end_state <- gen_game_end_state gt Fascist;
    send_message_to_listeners gt (GeneralRsp (GameEndState game_end_state))

fun game_loop (gt : game_table) =
    tt <- current_turn_state gt;
    now <- now;
    let fun action_not_overdue (delta : float) : bool =
            if gt.TimedGame
            then addSeconds gt.LastAction (ceil (delta * 60.)) > now
            else True
    in  if tt.LiberalPolicies = 5
        then liberals_win gt
        else
            if tt.FascistPolicies = 6
            then fascists_win gt
            else
                if not tt.ChancSelDone
                then
                    if action_not_overdue gt.ChanNomTime
                    then return {}
                    else punish_president gt
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
                            then if action_not_overdue gt.GovVoteTime
                                 then return {}
                                 else punish_non_voters gt
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
                             hitler_o <- oneOrNoRows1 (SELECT *
                                                       FROM hitler
                                                       WHERE hitler.Room = {[gt.Room]}
                                                         AND hitler.Game = {[gt.Game]}
                                                         AND hitler.Place =
                                                         {[tt.Chancellor]});
                             case hitler_o of
                                 None   => return {}
                               | Some _ => fascists_win gt);
                        if not tt.DiscardDone
                        then
                            if action_not_overdue gt.PresDisTime
                            then return {}
                            else punish_president gt
                        else
                            if not tt.EnactionDone
                            then if action_not_overdue gt.ChanEnaTime
                                 then return {}
                                 else punish_chancellor gt
                            else
                                if not tt.ExecActionDone
                                then if action_not_overdue gt.ExecActTime
                                     then return {}
                                     else punish_president gt
                                else return {}
    end

task periodic 1 = (* Loop for active games. *)
     fn {} =>
        games <- queryL1 (SELECT game.*
                          FROM (game
                              INNER JOIN room
                              ON  game.Room = room.Room
                              AND game.Game = room.CurrentGame));
        mapM_ game_loop games;
        return {}

task periodic 60 = (* Kick removal loop *)
     fn {} =>
        now <- now;
        dml (DELETE FROM kick WHERE Till < {[now]})

fun admin_view {} : transaction page =
    let fun submit_admin_view {} : transaction page = return <xml></xml>
    in  pt <- check_role Admin;
        return <xml></xml>
    end
