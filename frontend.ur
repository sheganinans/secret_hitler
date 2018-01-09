open Auth
open Protocol
open Tables
open Utils

fun on_load {} : transaction {} =
    me <- self;
    chan <- oneRow1 (SELECT player_in_game.Chan
                     FROM player_in_game
                     WHERE player_in_game.Client = {[me]});
    return {}

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

and main_menu_body {} : xbody =
    <xml><table>
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

and chat (chat_id : int) : transaction xbody =
    return <xml></xml>

and view_room (room_id : int) : transaction page =
    (pt, rt) <- only_if_player_not_kicked_exn room_id;
    let fun join_game {} : transaction page =
            let fun submit_join_game (playing : { Playing : bool }) : transaction page =

                    gt_o <- oneOrNoRows1 (SELECT *
                                          FROM game
                                          WHERE game.Game = {[rt.CurrentGame]});
                    (case gt_o of
                         None => return {}
                       | Some gt =>
                         me <- self;
                         chan <- channel;
                         num_in_game <- number_of_players_in_room_for_game rt;
                         add_player_to_game rt pt me chan playing.Playing num_in_game);
                    view_room room_id

            in  if rt.InGame
                then submit_join_game {Playing = False}
                else return <xml><body><form><table>
                  <tr><th>Playing?</th><td><checkbox{#Playing}/></td></tr>
                  <tr><submit action={submit_join_game}/></tr>
                </table></form></body></xml>
            end

    in  rp_o <- oneOrNoRows1 (SELECT *
                              FROM room_player
                              WHERE room_player.Player = {[pt.Player]}
                                AND room_player.Room   = {[rt.Room]});
        case rp_o of
            None   => join_room room_id
          | Some _ =>
            (rs : source (option rule_set)) <- source None;

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

            (vs : source (option (list {Place : int, Vote : bool}))) <- source None;

            let fun no_game_yet {} : transaction xbody = return <xml></xml>

                fun join_game_view {} : transaction xbody = return <xml></xml>

                fun public_rsp_handler (rsp : Protocol.public_response) : transaction {} =
                    case rsp of
                        _ => return {}

                fun private_rsp_handler (rsp : Protocol.private_response) : transaction {} =
                    case rsp of
                        _ => return {}

                fun client_handler (msg : Protocol.in_game_response) : transaction {} =
                    let
                    in  case msg of
                            PublicRsp rsp => public_rsp_handler rsp
                          | PrivateRsp rsp => private_rsp_handler rsp
                    end

                fun display_vote_state {} : transaction xbody =
                    return <xml></xml>

            in  player_list <- get_all_un_and_id_in_room room_id;
                pig <- oneOrNoRows1 (SELECT *
                                     FROM player_in_game
                                     WHERE player_in_game.Room = {[rt.Room]}
                                       AND player_in_game.Game = {[rt.CurrentGame]}
                                       AND player_in_game.Player = {[pt.Player]});
                case pig of
                    None => join_game {}
                  | Some pig =>
                    chan <- channel;
                    dml (UPDATE player_in_game
                         SET Chan = {[chan]}
                         WHERE Room = {[rt.Room]}
                           AND Game = {[rt.CurrentGame]}
                           AND Player = {[pt.Player]});
                    return <xml><body onload={let fun loop {} =
                                                      msg <- recv chan;
                                                      client_handler msg;
                                                      loop {}
                                              in rpc (on_load {});
                                                 loop {}
                                              end}><table>
                      <tr><th>View Room {[show rt.Nam]}</th></tr>
                      {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                                 player_list}
                    </table></body></xml>
            end
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

             mapM_ (fn (i,p) => dml (INSERT INTO table_ordering (Room, Game, InGameId, Place)
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

task periodic 60 = (* Kick removal loop *)
     fn {} =>
        now <- now;
        dml (DELETE FROM kick WHERE Till < {[now]})
