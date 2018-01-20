open Auth
open Protocol
open Tables
open Utils

fun on_view_room_load (rt : room_table) (pt : player_table) : transaction {} =
    chan <- oneRow1 (SELECT player_in_game.Chan
                     FROM player_in_game
                     WHERE player_in_game.Room = {[rt.Room]}
                       AND player_in_game.Game = {[rt.CurrentGame]}
                       AND player_in_game.Player = {[pt.Player]});
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

    in  return <xml><body><form><table>
      <tr><th>Signup</th></tr>
      <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
      <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
      <tr><th/><td><submit action={submit_signup}/></td></tr>
    </table></form></body></xml>
    end


and login_page {} : transaction page = return <xml><body>{login_form {}}</body></xml>

and login_form {} : make_form = <xml><table>
  <tr><td><a link={main_menu {}}>Back to Main Menu</a></td></tr>
  <tr><td><a link={signup_page {}}>Need to Signup?</a></td></tr>
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

and check_login (r : role) : transaction (result player_table string) =
    let val err = "Please login."
    in c <- getCookie username_and_pass;
       case c of
           None   => return (Err err)
         | Some c =>
           p_o <- oneOrNoRows1 (SELECT *
                                FROM player
                                WHERE player.Username = {[c.Username]}
                                  AND player.PassHash = {[c.PassHash]});
           case p_o of
               None   => return (Err err)
             | Some p =>
               if p.PassHash = c.PassHash
               then let fun check r b =
                            if not b
                            then return (Err <| "Must be " ^ show r ^ " to do that.")
                            else return (Ok p)
                    in check r (case r of Admin  => is_admin c.Username
                                        | Player => True)
                    end
               else return (Err err)
    end

and check_role (role : role) : transaction player_table =
    check <- check_login role;
    case check of
        Err (_ : string) => error <xml>{login_form {} }</xml>
      | Ok   pt          => return pt

and room_exists_exn (room_id : int) : transaction room_table =
    rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case rt_o of
        None => error <xml>(*{main_menu_body {}}*)</xml>
      | Some rt => return rt

and player_in_room_exn (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Room   = {[rt.Room]}
                            AND room_player.Player = {[pt.Player]});
    case rp_o of
        None => error <xml>(*{main_menu_body ()}*)</xml>
      | Some _ => return (pt, rt)

and only_if_owner_mod_exn (room_id : int) : transaction (player_table * room_table) =
    (pt, rt) <- player_in_room_exn room_id;
    m <- oneOrNoRows1 (SELECT *
                       FROM mod
                       WHERE mod.Room   = {[room_id]}
                         AND mod.Player = {[pt.Player]});
    if rt.OwnedBy = pt.Player || m <> None
    then return (pt, rt)
    else error <xml>(*{main_menu_body {}}*)</xml>

and only_if_player_not_kicked_exn (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    is_kicked <- oneOrNoRows1 (SELECT *
                               FROM kick
                               WHERE kick.Player = {[pt.Player]}
                                 AND kick.Room = {[rt.Room]});
    case is_kicked of
        Some _ => error <xml>(*{kicked_body rt}*)</xml>
      | None => return (pt, rt)


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


and select_rooms_controlled {} : transaction (list room_table) =
    pt <- check_role Player;
    rl_1 <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
    rl_2 <- queryL1 (SELECT room.*
                     FROM (room
                         INNER JOIN mod
                         ON room.Room   = mod.Room
                         AND mod.Player = {[pt.Player]}));
    return (List.append rl_1 rl_2)


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
    return <xml><body>{main_menu_body {}}</body></xml>

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

and submit_new_game (room_id : int) (rule_set : rule_set) : transaction page =
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
                  , CurrentTurn
                  , GameStarted
                  , LastAction
                  , GameEnded )
              VALUES
                ( {[rt.Room]}
                  , {[rt.CurrentGame]}
                  , 0
                  , NULL
                  , {[now]}
                  , NULL )));
    set_rules rt rule_set;
    view_room rt.Room

and new_game_page (room_id : int) {} : transaction page =
    return <xml><body><form><table>
      <tr><th>New Game: Time Table</th></tr>
      <tr><th>Kill Player as Punishment?</th><td><checkbox{#KillPlayer}/></td></tr>
      <tr><th>Timed Game?</th>               <td><checkbox{#TimedGame}/></td></tr>
      <tr><th>Chancellor Nomination</th>     <td><number{#ChanNomTime} value=0./></td></tr>
      <tr><th>Government Vote</th>           <td><number{#GovVoteTime} value=0./></td></tr>
      <tr><th>President Discard</th>         <td><number{#PresDisTime} value=0./></td></tr>
      <tr><th>Chancellor Enaction</th>       <td><number{#ChanEnaTime} value=0./></td></tr>
      <tr><th>Executive Action</th>          <td><number{#ExecActTime} value=0./></td></tr>
      <tr><td><submit action={submit_new_game room_id}/></td></tr>
    </table></form></body></xml>

and new_game {} : transaction page =
    room_list <- select_rooms_controlled {};
    return <xml><body>
      {List.mapX (fn r => <xml><form><submit value={r.Nam}
                                             action={new_game_page r.Room}/>
                          </form></xml>)
                 room_list}
    </body></xml>

and view_your_rooms {} : transaction page =
    rl <- select_rooms_controlled {};
    case rl of
        [] => main_menu {}
      | rl =>
        return <xml><body>
          <table>{List.mapX (fn r => <xml><tr><td>
                                       <a link={view_room r.Room}>
                                         {[r.Nam]}</a></td></tr></xml>)
                            rl}
        </table></body></xml>

and view_public_rooms {} : transaction page =
    rl <- queryL1 (SELECT * FROM room WHERE room.Pass = NULL);
    return <xml><body><table>
      <tr><td><a link={main_menu {}}>Main Menu</a></td></tr>
      {List.mapX (fn r => <xml><tr><td>
                            <a link={join_room r.Room}>
                              {[show r.Nam]}</a></td></tr></xml>)
                 rl}
    </table></body></xml>

and view_invite (room_id : int) : transaction page =
    (pt, rt) <- player_in_room_exn room_id;
    return <xml><body><table>
      <tr><th><a link={view_room room_id}>Back to {[rt.Nam]}</a></th></tr>
      <tr><th>Link:</th><td>
        <a link={view_room room_id}>
          {[Consts.website_url]}{[show (url (view_room room_id))]}</a></td></tr>
      {case rt.Pass of None      => <xml></xml> : xtable
                     | Some pass => <xml><tr><th>Pass:</th><td>{[pass]}</td></tr></xml>}
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
    (pt, rt) <- only_if_player_not_kicked_exn room_id;
    let fun submit_join_game (playing : bool) {} : transaction page =
            gt_o <- oneOrNoRows1 (SELECT *
                                  FROM game
                                  WHERE game.Game = {[rt.CurrentGame]});
            (case gt_o of
                 None => return {}
               | Some gt =>
                 me <- self;
                 chan <- channel;
                 num_in_game <- number_of_players_in_room_for_game rt;
                 add_player_to_game rt pt me chan playing num_in_game);
            view_room room_id

    in  if rt.InGame
        then submit_join_game False {}
        else return <xml><body>
          <table>
            <tr><td><form>
              <submit value="Playing"  action={submit_join_game  True}/></form></td></tr>
            <tr><td><form>
              <submit value="Watching" action={submit_join_game False}/></form></td></tr>
        </table></body></xml>
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
        ordering <- queryL1 (SELECT *
                             FROM table_ordering
                             WHERE table_ordering.Room = {[rt.Room]}
                               AND table_ordering.Game = {[rt.CurrentGame]});

        let fun no_game_yet {} : transaction xbody = return <xml>No Game Yet</xml>

        in mods <- queryL1 (SELECT * FROM mod WHERE mod.Room = {[rt.Room]});

            curr_game <- oneOrNoRows1 (SELECT *
                                       FROM game
                                       WHERE game.Room = {[rt.Room]}
                                         AND game.Game = {[rt.CurrentGame]});
            case curr_game of
                None => if rt.OwnedBy = pt.Player ||
                           List.exists (fn m => pt.Player = m.Player) mods
                        then new_game_page room_id {}
                        else return <xml><body>
                          <a link={view_invite room_id}>View Invite</a>
                          <br/>No Game yet</body></xml>
              | Some _ =>
                pig <- oneOrNoRows1 (SELECT *
                                     FROM player_in_game
                                     WHERE player_in_game.Room = {[rt.Room]}
                                       AND player_in_game.Game = {[rt.CurrentGame]}
                                       AND player_in_game.Player = {[pt.Player]});
                case pig of
                    None => join_game room_id
                  | Some pig =>
                    chan <- channel;
                    dml (UPDATE player_in_game
                         SET Chan = {[chan]}
                         WHERE Room = {[rt.Room]}
                           AND Game = {[rt.CurrentGame]}
                           AND Player = {[pt.Player]});

                    (client_body, client_handler) <- Client_handler.client_view_closure {};

                    return <xml><body onload={
                      let fun rsp_loop {} =
                              msg <- recv chan;
                              client_handler msg;
                              rsp_loop {}
                      in  rpc (on_view_room_load rt pt);
                          spawn (rsp_loop {})
                      end}><table>
                      <tr><td><a link={view_invite room_id}>View Invite</a></td></tr>
                      <tr><td><active code={c_b <- get client_body;
                                            return c_b}></active></td></tr></table></body></xml>
        end

and get_all_un_and_id_in_room (room_id : int) : transaction (list player_id_and_username) =
    _ <- player_in_room_exn room_id;
    queryL1 (SELECT player.Player, player.Username
             FROM (room_player
                 INNER JOIN player
                 ON room_player.Player = player.Player)
             WHERE room_player.Room = {[room_id]}
             ORDER BY player.Username)

and new_mod (room_id : int) : transaction page =
    _ <- only_if_owner_mod_exn room_id;
    let fun submit_new_mod (room_id : int) (player_id : int) {} =
            (pt, rt) <- only_if_owner_mod_exn room_id;
            now <- now;
            dml (INSERT INTO mod (Room, Player, SetBy, Time)
                 VALUES ({[rt.Room]}, {[player_id]}, {[pt.Player]}, {[now]}));
            view_room room_id

    in  player_list <- get_all_un_and_id_in_room room_id;
        return <xml><body>
          {List.mapX (fn p =>
                         <xml><form>
                           <submit value={p.Username}
                                   action={submit_new_mod room_id p.Player}/>
                         </form></xml>)
                     player_list}</body></xml>
    end

and rem_mod (room_id : int) : transaction page =
    _ <- only_if_owner_mod_exn room_id;
    let fun submit_rem_mod (room_id : int) (player_id : int) {} =
            (pt, rt) <- only_if_owner_mod_exn room_id;
            now <- now;
            dml (DELETE FROM mod WHERE Room = {[room_id]}
                                   AND Player = {[player_id]});
            view_room room_id

    in  player_list <- get_all_un_and_id_in_room room_id;
        return <xml><body>
          {List.mapX (fn p =>
                         <xml><form>
                           <submit value={p.Username}
                                   action={submit_rem_mod room_id p.Player}/>
                         </form></xml>)
                     player_list}</body></xml>
    end
