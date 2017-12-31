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

and login_form {} : make_form = <xml><table>
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

and login_page {} : transaction page = return <xml><body>{[login_form {}]}</body></xml>

and check_role (role : role) : transaction player_table =
    check <- Auth.check_login role;
    case check of
        Err (_ : string) => error <xml>{[login_form {}]}</xml>
      | Ok   pt          => return pt

and only_if_owner_mod_admin (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case rt_o of
        None => error <xml>Room doesn't exist!</xml>
      | Some rt =>
        m <- oneOrNoRows1 (SELECT *
                           FROM mod
                           WHERE mod.Room   = {[room_id]}
                             AND mod.Player = {[pt.Player]});
        if rt.OwnedBy = pt.Player || m <> None || is_admin pt.Username
        then return (pt, rt)
        else error <xml>You don't own {[rt.Nam]}!</xml>

and select_rooms_controlled {} : transaction (list room_table) =
    pt <- check_role Player;
    rl_1 <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
    rl_2 <- queryL1 (SELECT room.*
                     FROM (room
                         INNER JOIN mod
                         ON room.Room = mod.Room
                         AND mod.Player = {[pt.Player]}));
    return (List.append rl_1 rl_2)

and if_room_exists (room_id : int) (page_f : room_table -> transaction page) : transaction page =
    rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case rt_o of
        None => main_menu {}
      | Some rt => page_f rt

and main_menu {} : transaction page =
    _ <- check_role Player;
    return <xml><body>
      <table>
        <tr><td><a link={new_room {}}>New Room</a></td></tr>
        <tr><td><a link={new_game {}}>New Game</a></td></tr>
        <tr><td><a link={view_room None}>View Rooms</a></td></tr>
      </table></body></xml>

and new_room {} : transaction page =
    let fun submit_new_room room_form : transaction page =
            pt      <- check_role Player;
            now     <- now;
            room_id <- nextval room_seq;
            rand    <- (if room_form.Private
                        then rand <- rand; return (Some <| show rand)
                        else               return None);
            dml (INSERT INTO room (Room, OwnedBy, Nam, Pass, CurrentGame)
                 VALUES
                   ( {[room_id]}
                   , {[pt.Player]}
                   , {[room_form.RoomName]}
                   , {[rand]}
                   , 0 ));
            dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                 VALUES
                   ( {[room_id]}
                   , {[pt.Player]}
                   , {[pt.Player]}
                   , {[now]} ));
            show_room room_id

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
            (pt, rt) <- only_if_owner_mod_admin room_id;
            ro <- oneOrNoRows1 (SELECT *
                                FROM game
                                WHERE game.Room = {[rt.Room]}
                                  AND game.Game = {[rt.CurrentGame]});
            (case ro of
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
                   , {[None]} )));
            show_room rt.Room

        fun new_game_page (room_id : int) {} : transaction page =
            return <xml><body><form>
              <table>
                <tr>New Game: Time Table</tr>
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

and view_room (room_id_o : option int) : transaction page = case room_id_o of
        Some room_id => show_room room_id
      | None         =>
        rl <- select_rooms_controlled {};
        case rl of
            [] => main_menu {}
          | rl =>
            return <xml><body>
              <table>
                <tr><td>View Rooms</td></tr>
                <tr><td>
                  <table>
                    {List.mapX (fn r =>
                                   <xml><tr><td>
                                     <a link={view_room (Some r.Room)}>{[r.Nam]}</a>
                                   </td></tr></xml>)
                               rl}
                  </table></td></tr>
              </table></body></xml>

and show_room (room_id : int) : transaction page =
    if_room_exists room_id (fn rt =>
        pt <- check_role Player;
        room_user_relation_o <-
            oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Player = {[pt.Player]}
                            AND room_player.Room   = {[room_id]});
        case room_user_relation_o of
            None   => join_room room_id
          | Some _ =>
            is_banned <-
                oneOrNoRows1 (SELECT *
                              FROM ban
                              WHERE ban.Player = {[pt.Player]}
                                AND ban.Room   = {[room_id]});
            case is_banned of
                Some _ => main_menu {} (*Some <| "Banned from room: " ^ rt.Nam*)
              | None   =>
                player_list_o <- get_all_usernames_and_ids_in_room room_id;
                case player_list_o of
                    None => main_menu {}
                  | Some player_list =>
                    return <xml><body><table>
                      <tr><td>View Room {[show rt.Nam]}</td></tr>
                      {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                                 player_list}
                    </table></body></xml>)

and get_all_usernames_and_ids_in_room (room_id : int)
    : transaction (option (list player_id_and_username)) =
    pt <- check_role Player;
    r_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case r_o of
        None    => return None
      | Some rt =>
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
            (pt, rt) <- only_if_owner_mod_admin room_id;
            now <- now;
            dml (INSERT INTO mod (Room, Player, SetBy, Time)
                 VALUES ({[rt.Room]}, {[player_id]}, {[pt.Player]}, {[now]}));
            main_menu {}

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

and join_room (room_id : int) : transaction page =
    let fun submit_private_room_secret (room_id : int)
                                       (pass : {Pass : string}) : transaction page =
            if_room_exists room_id (fn rt =>
                (if rt.Pass = None || rt.Pass = Some pass.Pass
                 then pt  <- check_role Player;
                      now <- now;
                      dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                           VALUES ({[room_id]}, {[pt.Player]}, {[pt.Player]}, {[now]}))
                 else return {});
                show_room room_id)

    in  pt <- check_role Player;
        rp_o <- oneOrNoRows1 (SELECT *
                              FROM room_player
                              WHERE room_player.Player = {[pt.Player]}
                                AND room_player.Room = {[room_id]});
        case rp_o of
            Some _ => show_room room_id
          | None   =>
            if_room_exists room_id (fn rt =>
                case rt.Pass of
                    None =>
                    now <- now;
                    dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                         VALUES ({[room_id]}, {[pt.Player]}, {[pt.Player]}, {[now]}));
                    show_room room_id
                  | Some _ =>
                    return <xml><body><form><table>
                      <tr><th>Pass</th><td><password{#Pass}/></td></tr>
                      <tr><td><submit action={submit_private_room_secret room_id}/></td></tr>
                    </table></form></body></xml>)
    end

and admin_view {} : transaction page =
    let fun submit_admin_view {} : transaction page = return <xml></xml>
    in  _ <- check_role Admin;
        return <xml></xml>
    end
