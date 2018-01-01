open Auth
open Tables

fun role_closure [a]
                 (role : role)
                 (pf : player_table -> transaction a)
                 (err_ret : string -> transaction a)
    : transaction a =
    check <- Auth.check_login role;
    case check of
        Err err => err_ret err
      | Ok   pt => pf pt

fun only_if_owner_mod_admin_closure [a]
                                    (fun_name : string)
                                    (room_id : int)
                                    (pt : player_table)
                                    (ok_f : player_table * room_table -> transaction a)
                                    (err_f : string -> transaction a)
    : transaction a =
    r <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    m <- oneOrNoRows1 (SELECT *
                       FROM mod
                       WHERE mod.Room   = {[room_id]}
                         AND mod.Player = {[pt.Player]});
    case r of
        None => err_f "Room doesn't exist!"
      | Some r => if r.OwnedBy = pt.Player || m <> None || is_admin pt.Username
                  then ok_f (pt,r)
                  else err_f <| "You don't own " ^ r.Nam ^ "!"

fun debug_and_show_err [a] [b]
                       (fun_name : string)
                       (username : string)
                       (err : string)
    : transaction (xml a b []) =
    let val frontend_err = "Frontend." ^ fun_name ^ ": {" ^ username ^ "}: " ^ err
    in  debug frontend_err; return <xml>{[frontend_err]}</xml>
    end


fun signup_form (submit_action : player_name_and_pass -> transaction page) : xbody =
    <xml><form>
      <table>
        <tr><th>Signup</th></tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_action}/></td></tr>
      </table></form></xml>

fun main_menu (msgo : option string) : transaction page = player_page
    (fn pt =>
        return <xml><body><table>
          {case msgo of
               None     => <xml></xml>
             | Some msg => <xml><tr><td>{[msg]}</td></tr></xml>}
          <tr><td>
            <table>
              <tr><td><a link={new_room ()}>New Room</a></td></tr>
              <tr><td><a link={new_game ()}>New Game</a></td></tr>
              <tr><td><a link={view_room None}>View Rooms</a></td></tr>
              {if is_admin pt.Username
               then <xml><tr><td><a link={admin_test_page ()}>Admin</a></td></tr></xml>
               else <xml></xml>}
            </table></td></tr></table></body></xml>)

and submit_signup
        (set_cookie : bool)
        (signup : player_name_and_pass) : transaction page =
    let val pw_hs = Auth.basic_password_hash signup.PassHash
    in uo <- oneOrNoRows1 (SELECT player.Username
                           FROM player
                           WHERE player.Username = {[signup.Username]});

       case uo of
           Some u => login_page (Some <| "Username: " ^ u.Username ^ " already taken!")
         | None =>
           player_id <- nextval player_seq;

           dml (INSERT INTO player (Player, Username, PassHash)
                VALUES ({[player_id]}, {[signup.Username]}, {[pw_hs]}));

           (if set_cookie
            then set_username_cookie signup.Username pw_hs
            else return ());

           main_menu None
    end

and signup_page () : transaction page =
    return <xml><body>{signup_form (submit_signup True)}</body></xml>

and submit_login (login : player_name_and_pass) : transaction page =
    let val pw_hs = Auth.basic_password_hash login.PassHash
    in  hpo <- oneOrNoRows1 (SELECT player.PassHash
                             FROM player
                             WHERE player.Username = {[login.Username]});
        case hpo of
            None => login_page (Some "Login Failed!")
          | Some hp =>
            if hp.PassHash <> pw_hs
            then login_page (Some "Login Failed!")
            else set_username_cookie login.Username pw_hs;
                 main_menu None
    end

and login_form (msgo : option string) : xbody =
    <xml>
      <table>
        <tr><td><a link={signup_page ()}>Need to signup?</a></td></tr>
        {case msgo of
             None => <xml></xml>
           | Some msg => <xml><tr><td>{[msg]}</td></tr></xml>}
        <tr><td>
          <form>
            <table>
              <tr>Login!</tr>
              <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
              <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
              <tr><th/><td><submit action={submit_login}/></td></tr>
            </table></form></td></tr>
      </table></xml>

and login_page (msgo : option string) : transaction page =
    return <xml><body>{login_form msgo}</body></xml>

and admin_page (pf : player_table -> transaction page) : transaction page =
    role_closure Admin pf (fn err => return <xml>{[err]}</xml>)

and admin_form (pf : player_table -> transaction make_form) : transaction make_form =
    role_closure Admin pf (fn err => return <xml>{[err]}</xml>)

and admin_query [a] (pf : player_table -> transaction (option a)) : transaction (option a) =
    role_closure Admin pf (fn err => return None)

and player_page (pf : player_table -> transaction page) : transaction page =
    role_closure Player pf (fn err => login_page (Some err))

and player_form (pf : player_table -> transaction make_form) : transaction make_form =
    role_closure Player pf (fn err => return <xml>{[err]}</xml>)

and only_if_owner_mod_admin_page (fun_name : string)
                                 (room_id : int)
                                 (page_f : player_table * room_table -> transaction page)
    : transaction page =
    player_page
        (fn pt =>
            only_if_owner_mod_admin_closure
                fun_name room_id pt page_f (debug_and_show_err fun_name pt.Username))

and only_if_owner_mod_admin_form (fun_name : string)
                                 (room_id : int)
                                 (page_f : player_table * room_table -> transaction make_form)
    : transaction make_form =
    player_form
        (fn pt =>
            only_if_owner_mod_admin_closure
                fun_name room_id pt page_f (debug_and_show_err fun_name pt.Username))

and submit_new_room (player_id : int) room_form : transaction page =
    room_id <- nextval room_seq;
    rand <- rand;
    now <- now;
    dml (INSERT INTO
           room (Room, OwnedBy, Nam, Pass, CurrentGame, InGame)
         VALUES
           ( {[room_id]}
           , {[player_id]}
           , {[room_form.RoomName]}
           , {[if room_form.Private then Some (show rand) else None]}
           , 0
           , FALSE ));
    dml (INSERT INTO room_player (Room, Player, SetBy, Time)
         VALUES ({[room_id]}, {[player_id]}, {[player_id]}, {[now]}));
    view_room (Some room_id)

and new_room_form (player_id : int) : make_form =
    <xml><form>
      <table>
        <tr><th>New Room</th></tr>
        <tr><th>Name</th><td><textbox{#RoomName}/></td></tr>
        <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
        <tr><td><submit action={submit_new_room player_id}/></td></tr>
      </table></form></xml>

and new_room () : transaction page =
    player_page (fn pt => return <xml><body>{new_room_form pt.Player}</body></xml>)

and submit_new_game (room_id: int) (game_form : game_time_table) : transaction page =
    only_if_owner_mod_admin_page "submit_new_game" room_id
        (fn (pt,rt) =>
            ro <- oneOrNoRows1 (SELECT *
                                FROM game
                                WHERE game.Room = {[rt.Room]}
                                  AND game.Game = {[rt.CurrentGame]});
            (case ro of
                Some _ => return ()
              | None => now <- now;
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
                   , GameEnded)
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
                   , {[None]})));
            view_room (Some rt.Room))

and new_game_form (room_id : int) : make_form =
    <xml><form>
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
      </table></form></xml>

and new_game_page (room_id : int) : transaction page =
    only_if_owner_mod_admin_page "new_game_page" room_id
        (fn _ =>
            return <xml><body>{new_game_form room_id}</body></xml>)

and submit_choose_room_for_game (room_id : int) () : transaction page =
    only_if_owner_mod_admin_page "submit_choose_room_for_game" room_id (fn (pt,rt) => new_game_page rt.Room)

and choose_room_for_game (room_list : list room_table) : make_form =
    choose_room_form_closure room_list submit_choose_room_for_game

and new_game () : transaction page = player_page
    (fn pt =>
        rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
        case rl of
            [] => new_room ()
          | rl => return <xml><body>New Game<br/>{choose_room_for_game rl}</body></xml>)

and submit_new_mod (room_id : int) (player_id : int) () : transaction page =
    only_if_owner_mod_admin_page "submit_new_mod" room_id
        (fn (pt,rt) =>
            now <- now;
            dml (INSERT INTO mod (Room, Player, SetBy, Time)
                 VALUES ({[rt.Room]}, {[player_id]}, {[pt.Player]}, {[now]}));
            main_menu (Some "New Mod successfully added!"))

and make_mod_form (room_id : int)
                  (player_list : list player_id_and_username)
    : make_form =
    List.mapX (fn p =>
                  <xml><form>
                    <submit
                      value={p.Username}
                      action={submit_new_mod room_id p.Player}/></form></xml>)
              player_list

and get_all_rooms_users room_id (* TODO: make return result for if no permission! *)
  : transaction (list player_id_and_username) =
    queryL1 (SELECT player.Player, player.Username
             FROM (room_player
                 INNER JOIN player
                 ON room_player.Player = player.Player)
             WHERE room_player.Room = {[room_id]})

and get_all_usernames_and_ids_in_room (room_id : int) : transaction (option (list player_id_and_username)) =
    admin_query
        (fn pt =>
            q <- queryL1 (SELECT player.Player, player.Username
                     FROM (room_player
                         INNER JOIN player
                         ON room_player.Player = player.Player)
                     WHERE room_player.Room = {[room_id]});
            return (Some q))

and new_mod_page (room_id : int) : transaction page =
    only_if_owner_mod_admin_page "new_mod_page" room_id
        (fn (pt,rt) =>
            player_list <- get_all_rooms_users room_id;
            return <xml><body>{make_mod_form room_id player_list}</body></xml>)

and submit_choose_room_for_mod (room_id : int) () : transaction page =
    only_if_owner_mod_admin_page "submit_choose_room_for_mod" room_id (fn (pt,rt) => new_mod_page rt.Room)

and choose_room_form_closure (room_list : list room_table)
                             (submit_action : int -> {} -> transaction page)
    : make_form =
    List.mapX (fn r => <xml><form>
                         <submit
                           value={r.Nam}
                           action={submit_action r.Room}/>
                         </form></xml>)
              room_list

and choose_room_for_mod (room_list : list room_table) : make_form =
    choose_room_form_closure room_list submit_choose_room_for_mod

and view_room_list () : transaction page = player_page
    (fn pt =>
        rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
        (case rl of
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
               </table></body></xml>))

and view_room (room_id_o : option int) : transaction page = player_page
    (fn pt =>
        case room_id_o of
            None => view_room_list ()
          | Some room_id => show_room room_id)

(* Shortened sharable link. *)
and show_room (room_id : int) : transaction page = player_page
    (fn pt =>
        ro <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
        case ro of
            None    => new_room ()
          | Some rt =>
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
                    Some _ => main_menu (Some <| "Banned from room: " ^ rt.Nam)
                  | None   =>
                    player_list_o <- get_all_usernames_and_ids_in_room room_id;
                    case player_list_o of
                        None => return <xml>Can't do that!</xml>
                      | Some player_list =>
                        return <xml><body><table>
                          <tr><td>View Room {[show rt.Nam]}</td></tr>
                          {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                                     player_list}
                        </table></body></xml>)

and join_room room_id : transaction page = player_page
    (fn _ =>
        return <xml>Join Room</xml>)

and submit_add_user_to_room p_r : transaction page =
    let val   room_id = readError p_r.Room.Selection
        val player_id = readError p_r.Player.Selection
    in  only_if_owner_mod_admin_page "submit_add_user_to_room" room_id
            (fn (pt,rt)=>
                ro <- oneOrNoRows1 (SELECT *
                                    FROM room_player
                                    WHERE room_player.Room = {[room_id]}
                                      AND room_player.Player = {[player_id]});
                (case ro of
                    Some _ => return ()
                  | None =>
                    now <- now;
                    dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                         VALUES ({[room_id]}, {[player_id]}, {[pt.Player]}, {[now]})));
                admin_test_page ())
    end

and admin_test_page () : transaction page =
    let fun make_add_user_to_room_form () : transaction make_form = admin_form (fn pt =>
            players <- queryL1 (SELECT * FROM player);
            rooms   <- queryL1 (SELECT * FROM room);
            return <xml><table>
              <tr><th>Add User to Room</th></tr>
              <tr><td><form>
                <subform {#Player}>
                  <select {#Selection}>
                    {List.mapX (fn p =>
                                   <xml><option value={show p.Player}>
                                     {[p.Username]}</option></xml>) players}
                  </select>
                </subform>
                <subform {#Room}>
                  <select {#Selection}>
                    {List.mapX (fn r =>
                                   <xml><option value={show r.Room}>
                                     {[r.Nam]}</option></xml>) rooms}
                  </select>
                </subform>
                <submit action={submit_add_user_to_room}/></form></td></tr>
            </table></xml>)

        fun make_new_game_form () : transaction make_form = admin_form
            (fn pt =>
                rl <- queryL1 (SELECT * FROM room);
                case rl of
                    [] => return (new_room_form pt.Player)
                  | rl => return <xml><table>
                                   <tr><th>Choose Room for Game</th></tr>
                                   <tr><td>{choose_room_for_game rl}</td></tr></table></xml>)

        fun submit_add_user_to_game room_id form =
            return <xml></xml>

        fun add_user_to_game (is_watching : bool) (user_to_game : string)
            : transaction make_form =
            let fun add_user room_id {} = admin_page (fn pt =>
                    room <- queryL1 (SELECT * FROM room WHERE room.Room = {[room_id]});
                    return <xml>
                      <body>
                        <form>
                          <table>
                            <tr><th>User</th><td></td></tr>
                            <tr><submit action={submit_add_user_to_game room_id}/></tr>
                          </table>
                        </form>
                      </body>
                    </xml>)
            in  admin_form (fn pt =>
                               room_list <- queryL1 (SELECT * FROM room);
                               return (choose_room_form_closure room_list add_user))
            end

        fun game () : transaction make_form =
            let fun new_game_admin_view (room_id : int) () : transaction page = admin_page
                    (fn pt =>
                        rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
                        case rt_o of
                            None => admin_test_page ()
                          | Some rt =>
                            player_list_o <- get_all_usernames_and_ids_in_room room_id;
                            case player_list_o of
                                None => return <xml>You can't do that!</xml>
                              | Some player_list =>
                                return <xml><body><table>
                                  <tr><td>View Room {[rt.Nam]}</td></tr>
                                  {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                                             player_list}
                                </table></body></xml>)
            in  admin_form
                    (fn pt =>
                        room_list <- queryL1 (SELECT * FROM room);
                        return (choose_room_form_closure room_list new_game_admin_view))
            end

        val line_row = <xml><tr><td><hr/></td></tr></xml>

    in  admin_page
            (fn pt =>
                add_user_to_room_form <- make_add_user_to_room_form ();
                new_game_form <- make_new_game_form ();
                game <- game ();
                return <xml><body><table>
                  <tr><td>{signup_form (submit_signup False)}</td></tr>
                  {line_row}
                  <tr><td>{new_room_form pt.Player}</td></tr>
                  {line_row}
                  <tr><td>{add_user_to_room_form}</td></tr>
                  {line_row}
                  <tr><td>{game}</td></tr>
                </table></body></xml>)
    end
