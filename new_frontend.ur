open Auth
open Tables

fun signup_page {} : transaction page =
    let fun submit_signup (signup : player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_password_hash signup.PassHash
            in  uo <- oneOrNoRows1 (SELECT player.Username
                                    FROM player
                                    WHERE player.Username = {[signup.Username]});
                case uo of
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
    let val pw_hs = Auth.basic_password_hash login_form.PassHash
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

and banned (room_id : int) : transaction page =
    rt <- room_exists_exn room_id;
    return <xml><body><table>
      <tr><th>You have been banned from room: {[rt.Nam]}</th></tr>
      <tr>Appeal system coming soon!</tr>
      <tr><td><a link={main_menu {}}>Main Menu</a></td></tr>
    </table></body></xml>

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
                        then rand <- rand; return (Some (basic_password_hash (show rand)))
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
    let fun submit_new_game (room_id: int) (game_form : game_time_table) : transaction page =
            (pt, rt) <- only_if_owner_mod_admin_exn room_id;
            r_o <- oneOrNoRows1 (SELECT *
                                 FROM game
                                 WHERE game.Room = {[rt.Room]}
                                   AND game.Game = {[rt.CurrentGame]});
            (case r_o of
                Some _ => return {}
              | None   =>
                now <- now;
                dml (INSERT INTO game
                       ( Game
                       , Room
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
                   ( {[rt.CurrentGame]}
                   , {[rt.Room]}
                   , {[game_form.ChanNomTime]}
                   , {[game_form.GovVoteTime]}
                   , {[game_form.PresDisTime]}
                   , {[game_form.ChanEnaTime]}
                   , {[game_form.ExecActTime]}
                   , {[Some 0]}
                   , {[now]}
                   , {[now]}
                   , {[None]} )));
            view_room rt.Room

        fun new_game_page (room_id : int) {} : transaction page =
            return <xml><body><form>
              <table>
                <tr><th>New Game: Time Table</th></tr>
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
            <tr><td>
              <table>
                {List.mapX (fn r =>
                               <xml><tr><td>
                                 <a link={view_room r.Room}>{[r.Nam]}</a>
                               </td></tr></xml>)
                           rl}
            </table></td></tr>
        </table></body></xml>

and view_public_rooms {} : transaction page = return <xml></xml>

and view_invite (room_id : int) : transaction page =
    pt <- check_role Player;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Room = {[room_id]}
                            AND room_player.Player = {[pt.Player]});
    case rp_o of
        None   => main_menu {}
      | Some _ =>
        r_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
        case r_o of
            None => main_menu {}
          | Some rt =>
            return <xml><body><table>
              <tr><th><a link={view_room room_id}>Back to {[rt.Nam]}</a></th></tr>
              <tr><th>Link:</th><td>{[show (url (view_room room_id))]}</td></tr>
              {[case rt.Pass of
                    None => <xml></xml> : xtable
                  | Some pass => <xml><tr><th>Pass:</th><td>{[pass]}</td></tr></xml>]}
            </table></body></xml>

and join_room (room_id : int) : transaction page =
    let fun submit_private_room_secret (room_id : int)
                                       (pass : {Pass : string}) : transaction page =
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

and view_room (room_id : int) : transaction page =
    rt <- room_exists_exn room_id;
    pt <- check_role Player;
    is_banned <- oneOrNoRows1 (SELECT *
                               FROM ban
                               WHERE ban.Player = {[pt.Player]}
                                 AND ban.Room   = {[room_id]});
    case is_banned of
        Some _ => banned rt.Room
      | None   =>
        rp_o <- oneOrNoRows1 (SELECT *
                              FROM room_player
                              WHERE room_player.Player = {[pt.Player]}
                                AND room_player.Room   = {[room_id]});
        case rp_o of
            None   => join_room room_id
          | Some _ =>
            player_list_o <- get_all_usernames_and_ids_in_room room_id;
            case player_list_o of
                None => main_menu {}
              | Some player_list =>
                return <xml><body><table>
                  <tr><th>View Room {[show rt.Nam]}</th></tr>
                  {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                             player_list}
                </table></body></xml>

and start_game (room_id : int) : transaction page =
    (pt, rt) <- only_if_owner_mod_admin_exn room_id;
    player_list <- queryL1 (SELECT *
                            FROM player_in_game
                            WHERE player_in_game.Game = {[rt.CurrentGame]}
                              AND player_in_game.Room = {[rt.Room]});
    let val players = List.filter (fn p => not p.Watching) player_list
        val watchers = List.filter (fn p => p.Watching) player_list
    in
        return <xml></xml>
    end

and join_game (room_id : int) : transaction page =
    let fun submit_join_game (room_id : int)
                             (playing : {Playing : bool}) : transaction page =
            (pt, rt) <- player_in_room_exn room_id;
            gt_o <- oneOrNoRows1 (SELECT *
                                  FROM game
                                  WHERE game.Game = {[rt.CurrentGame]});
            (case gt_o of
                None => return {}
              | Some gt =>
                me <- self;
                chan <- channel;
                dml (INSERT INTO player_in_game
                       ( Game
                       , Room
                       , Player
                       , Client
                       , Chan
                       , Watching )
                     VALUES
                       ( {[rt.CurrentGame]}
                       , {[rt.Room]}
                       , {[pt.Player]}
                       , {[me]}
                       , {[chan]}
                       , {[not playing.Playing]} )));
            view_room room_id

    in  return <xml><body><form><table>
          <tr><th>Playing?</th><td><checkbox{#Playing}/></td></tr>
            <tr><submit action={submit_join_game room_id}/></tr>
        </table></form></body></xml>
    end

and get_all_usernames_and_ids_in_room (room_id : int)
    : transaction (option (list player_id_and_username)) =
    (pt, rt) <- player_in_room_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Room = {[rt.Room]}
                            AND room_player.Player = {[pt.Player]});
    case rp_o of
        None    => return None
      | Some rp =>
        q <- queryL1 (SELECT player.Player, player.Username
                      FROM (room_player
                          INNER JOIN player
                          ON room_player.Player = player.Player)
                      WHERE room_player.Room = {[rt.Room]});
        return (Some q)

and new_mod {} : transaction page =
    let fun submit_new_mod (room_id : int) (player_id : int) {} =
            (pt, rt) <- only_if_owner_mod_admin_exn room_id;
            now <- now;
            dml (INSERT INTO mod (Room, Player, SetBy, Time)
                 VALUES ({[rt.Room]}, {[player_id]}, {[pt.Player]}, {[now]}));
            view_room room_id

        fun new_mod_page (room_id : int) {} : transaction page =
            player_list_o <- get_all_usernames_and_ids_in_room room_id;
            case player_list_o of
                None             => main_menu {}
              | Some player_list =>
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
                          WHERE room_player.Player = {[pt.Player]}
                            AND room_player.Room = {[rt.Room]});
    case rp_o of
        None => error <xml>{main_menu_body ()}</xml>
      | Some _ => return (pt, rt)

and only_if_owner_mod_admin_exn (room_id : int) : transaction (player_table * room_table) =
    (pt, rt) <- player_in_room_exn room_id;
    m <- oneOrNoRows1 (SELECT *
                       FROM mod
                       WHERE mod.Room   = {[room_id]}
                         AND mod.Player = {[pt.Player]});
    if rt.OwnedBy = pt.Player || m <> None || is_admin pt.Username
    then return (pt, rt)
    else error <xml>{main_menu_body {}}</xml>

and select_rooms_controlled {} : transaction (list room_table) =
    pt <- check_role Player;
    rl_1 <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
    rl_2 <- queryL1 (SELECT room.*
                     FROM (room
                         INNER JOIN mod
                         ON room.Room = mod.Room
                         AND mod.Player = {[pt.Player]}));
    return (List.append rl_1 rl_2)

and admin_view {} : transaction page =
    let fun submit_admin_view {} : transaction page = return <xml></xml>
    in  pt <- check_role Admin;
        return <xml></xml>
    end

fun game_loop (gt : game_table) =
    turn <- oneRow1 (SELECT *
                     FROM turn
                     WHERE turn.Game = {[gt.Game]}
                       AND turn.Room = {[gt.Room]});
    players <- queryL1 (SELECT *
                        FROM player_in_game
                        WHERE player_in_game.Game = {[gt.Game]}
                          AND player_in_game.Room = {[gt.Room]});
    return {}

task periodic 1 = (* Loop for active games. *)
     fn () =>
        games <- queryL1 (SELECT game.*
                          FROM (game
                              INNER JOIN room
                              ON game.Room = room.Room
                              AND game.Game = room.CurrentGame));
        _ <- List.mapM game_loop games;
        return {}
