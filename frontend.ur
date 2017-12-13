open Auth
open Tables

fun role_closure
        [a]
        (role : role)
        (pf : player_table -> transaction a)
        (err_ret : string -> transaction a)
    : transaction a =
    check <- Auth.check_login role;
    case check of
        Err err => err_ret err
      | Ok   pt => pf pt

fun admin_page (pf : player_table -> transaction page) : transaction page =
    role_closure Admin pf (fn err => return <xml>{[err]}</xml>)

fun admin_form (pf : player_table -> transaction make_form) : transaction make_form =
    role_closure Admin pf (fn err => return <xml>{[err]}</xml>)

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

and submit_signup (signup : player_name_and_pass) : transaction page =
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

           set_username_cookie signup.Username pw_hs;

           main_menu None
    end

and signup_form (submit_action : player_name_and_pass -> transaction page) : xbody =
    <xml><form>
      <table>
        <tr>Signup!</tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_action}/></td></tr>
      </table></form></xml>

and signup_page () : transaction page =
    return <xml><body>{signup_form submit_signup}</body></xml>

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

and submit_new_room (player_id : int) room_form : transaction page =
    room_id <- nextval room_seq;
    rand <- rand;
    dml (INSERT INTO
           room (Room, OwnedBy, Nam, Pass, CurrentGame)
         VALUES
           ( {[room_id]}
           , {[player_id]}
           , {[room_form.RoomName]}
           , {[if room_form.Private then Some rand else None]}
           , 0 ));
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

and only_if_owner_mod_admin (fun_name : string)
                            (room_id : int)
                            (page_f : player_table * room_table -> transaction page) : transaction page =
    player_page
        (fn pt =>
            let fun debug_and_show_err err =
                    let val frontend_err =
                            "Frontend." ^ fun_name ^ ": {" ^ pt.Username ^ "}: " ^ err
                    in  debug frontend_err; return <xml>{[frontend_err]}</xml>
                    end
            in  r <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
                m <- oneOrNoRows1 (SELECT *
                                   FROM mod
                                   WHERE mod.Room   = {[room_id]}
                                     AND mod.Player = {[pt.Player]});
                case r of
                    None => debug_and_show_err "Room doesn't exist!"
                  | Some r => if r.OwnedBy <> pt.Player && m = None && not (is_admin pt.Username)
                              then debug_and_show_err <| "You don't own " ^ r.Nam ^ "!"
                              else page_f (pt,r)
            end)

and submit_new_game (room_id: int) (game_form : game_time_table) : transaction page =
    only_if_owner_mod_admin "submit_new_game" room_id
        (fn (pt,room) =>
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
                   , GameEnded)
                 VALUES
                   ( {[room.CurrentGame]}
                   , {[room.Room]}
                   , {[game_form.ChanNomTime]}
                   , {[game_form.GovVoteTime]}
                   , {[game_form.PresDisTime]}
                   , {[game_form.ChanEnaTime]}
                   , {[game_form.ExecActTime]}
                   , {[Some 0]}
                   , {[now]}
                   , {[None]}));
            view_room (Some room.Room))

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
    only_if_owner_mod_admin "new_game_page" room_id
        (fn _ =>
            return <xml><body>{new_room_form room_id}</body></xml>)

and submit_choose_room_for_game (room_id : int) {} : transaction page =
    only_if_owner_mod_admin "submit_choose_room_for_game" room_id (fn (pt,rt) => new_game_page rt.Room)

and choose_room_for_game (room_list : list room_table) : xbody =
    choose_room_form_closure room_list submit_choose_room_for_game

and new_game () : transaction page = player_page
    (fn pt =>
        rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
        case rl of
            [] => new_room ()
          | rl => return <xml><body>New Game<br/>{choose_room_for_game rl}</body></xml>)


and submit_new_mod (room_id : int) (player_id : int) {} : transaction page =
    only_if_owner_mod_admin "submit_new_mod" room_id
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
             FROM
               (room_player
                 INNER JOIN player
                 ON room_player.Player = player.Player)
             WHERE room_player.Room = {[room_id]})

and new_mod_page (room_id : int) : transaction page =
    only_if_owner_mod_admin "new_mod_page" room_id
        (fn (pt,rt) =>
            player_list <- get_all_rooms_users room_id;
            return <xml><body>{make_mod_form room_id player_list}</body></xml>)

and submit_choose_room_for_mod (room_id : int) {} : transaction page =
    only_if_owner_mod_admin "submit_choose_room_for_mod" room_id (fn (pt,rt) => new_mod_page rt.Room)

and choose_room_form_closure (room_list : list room_table)
                             (submit_action : int -> {} -> transaction page)
    : make_form =
    List.mapX (fn r => <xml><form>
                         <submit
                           value={r.Nam}
                           action={submit_action r.Room}/>
                         </form></xml>)
              room_list

and choose_room_for_mod (room_list : list room_table) : xbody =
    choose_room_form_closure room_list submit_choose_room_for_mod


and view_room (room_id_o : option int) : transaction page = player_page
    (fn pt =>
        case room_id_o of
            None =>
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
                  </table></body></xml>)
          | Some room_id =>
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
                      | None   => return <xml></xml>)

and join_room room_id : transaction page = player_page
    (fn _ =>
        return <xml></xml>)

and player_page (pf : player_table -> transaction page) : transaction page =
    role_closure Player pf (fn err => login_page (Some err))

and submit_add_user_to_room p_r : transaction page =
    let val   room_id = readError p_r.Room.Selection
        val player_id = readError p_r.Player.Selection
    in  only_if_owner_mod_admin "submit_add_user_to_room" room_id
            (fn (pt,rt)=>
                now <- now;
                dml (INSERT INTO room_player (Room, Player, SetBy, Time)
                     VALUES ({[room_id]}, {[player_id]}, {[pt.Player]}, {[now]}));
                admin_test_page ())
    end

and admin_test_page () : transaction page =
    let fun make_add_user_to_room_form () : transaction make_form = admin_form (fn pt =>
            players <- queryL1 (SELECT * FROM player);
            rooms   <- queryL1 (SELECT * FROM room);
            return <xml><form>
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
              <submit action={submit_add_user_to_room}/></form></xml>)

        fun make_new_game_form () : transaction make_form = admin_form
            (fn pt =>
                rl <- queryL1 (SELECT * FROM room);
                case rl of
                    [] => return (new_room_form pt.Player)
                  | rl => return (choose_room_for_game rl))

        fun add_user_to_game (is_watching : bool) (user_to_game : string)
            : transaction make_form = admin_form (fn pt =>
            return <xml><form></form></xml>)

        val line_row = <xml><tr><td><hr/></td></tr></xml>

    in  admin_page
            (fn pt =>
                add_user_to_room_form <- make_add_user_to_room_form ();
                new_game_form <- make_new_game_form ();
                return <xml><body><table>
                  <tr><td>{new_room_form pt.Player}</td></tr>
                  {line_row}
                  <tr><td><table>
                    <tr><th>Add User to Room</th>
                      <td>{add_user_to_room_form}</td>
                  </tr></table></td></tr>
                  {line_row}
                  <tr><td>{new_game_form}</td></tr>
                </table></body></xml>)
    end

fun game_loop (initial_state : Types.game) =
    let fun loop_it ((me,chan,state) : client * channel action * source Types.game)
            : transaction {} =
            let fun loop () =
                    sleep 1;
                    loop ()
            in loop () end
    in me <- self;
       chan <- channel;
       (*dml (INSERT INTO users (Client, Chan, Game)
              VALUES ({[me]}, {[chan]}, {[initial_state.Game]}));*)

       state <- source initial_state;
       loop_it (me,chan,state)
    end
