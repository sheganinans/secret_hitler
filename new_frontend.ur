open Auth
open Tables

fun select_rooms_controlled (player_id : int) : transaction (list room_table) =
    rl_1 <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[player_id]});
    rl_2 <- queryL1 (SELECT room.*
                     FROM (room
                         INNER JOIN mod
                         ON room.Room = mod.Room
                         AND mod.Player = {[player_id]}));
    return (List.append rl_1 rl_2)

fun get_all_rooms_users room_id (* TODO: make return result for if no permission! *)
  : transaction (list player_id_and_username) =
    queryL1 (SELECT player.Player, player.Username
             FROM (room_player
                 INNER JOIN player
                 ON room_player.Player = player.Player)
             WHERE room_player.Room = {[room_id]})

fun signup_page () : transaction page =
    let fun submit_signup (signup : player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_password_hash signup.PassHash
            in  uo <- oneOrNoRows1
                          (SELECT player.Username
                           FROM player
                           WHERE player.Username = {[signup.Username]});
                case uo of
                    Some u => login_page ()
                  | None =>
                    player_id <- nextval player_seq;
                    dml (INSERT INTO player (Player, Username, PassHash)
                         VALUES
                           ( {[player_id]}
                           , {[signup.Username]}
                           , {[pw_hs]} ));
                    set_username_cookie signup.Username pw_hs;
                    main_menu ()
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
            None => login_page ()
          | Some hp =>
            if hp.PassHash <> pw_hs
            then login_page ()
            else set_username_cookie login_form.Username pw_hs;
                 main_menu ()
    end

and login_form () : make_form = <xml><table>
  <tr><td><a link={signup_page ()}>Need to signup?</a></td></tr>
  <tr><td>
    <form>
      <table>
        <tr>Login!</tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_login}/></td></tr>
       </table></form></td></tr>
  </table></xml>

and login_page () : transaction page = return <xml><body>{[login_form ()]}</body></xml>

and check_role (role : role) : transaction player_table =
    check <- Auth.check_login role;
    case check of
        Err (_ : string) => error <xml>{[login_form ()]}</xml>
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
        else error <xml>{["You don't own " ^ rt.Nam ^ "!"]}</xml>

and main_menu () : transaction page =
    _ <- check_role Player;
    return <xml><body>
      <table>
        <tr><td><a link={new_room ()}>New Room</a></td></tr>
        <tr><td><a link={new_game ()}>New Game</a></td></tr>
        <tr><td><a link={view_room None}>View Rooms</a></td></tr>
      </table></body></xml>

and new_room () : transaction page =
    let fun submit_new_room room_form : transaction page =
            pt <- check_role Player;
            room_id <- nextval room_seq;
            rand <- (if room_form.Private
                     then rand <- rand; return (Some rand)
                     else return None);
            now <- now;
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
            view_room (Some room_id)
    in  return <xml><body><form>
          <table>
            <tr><th>New Room</th></tr>
            <tr><th>Name</th><td><textbox{#RoomName}/></td></tr>
            <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
            <tr><td><submit action={submit_new_room}/></td></tr>
          </table></form></body></xml>
    end

and new_game () : transaction page =
    let fun submit_new_game (room_id: int) (game_form : game_time_table) : transaction page =
            (pt, rt) <- only_if_owner_mod_admin room_id;
            ro <- oneOrNoRows1 (SELECT *
                                FROM game
                                WHERE game.Room = {[rt.Room]}
                                  AND game.Game = {[rt.CurrentGame]});
            (case ro of
                Some _ => return ()
              | None =>
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
            view_room (Some rt.Room)
    in  return <xml>(*<body><form>
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
      </table></form></body>*)</xml>
    end

and view_room (room_id_o : option int) : transaction page =
    let fun submit_view_game () : transaction page = return <xml></xml>
        fun view_room_list () : transaction page =
            pt <- check_role Player;
            rl <- select_rooms_controlled pt.Player;
            case rl of
                [] => new_room ()
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
    in  case room_id_o of
            None => view_room_list ()
          | Some room_id =>
            return <xml></xml>
    end

and join_room () : transaction page = return <xml></xml>

and admin_view () : transaction page =
    let fun submit_admin_view () : transaction page = return <xml></xml>
    in  return <xml></xml>
    end
