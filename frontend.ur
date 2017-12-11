open Auth
open Tables

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

and signup_form () : xbody =
    let fun submit_signup (signup : player_name_and_pass) : transaction page =
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
    in  <xml><form>
          <table>
            <tr>Signup!</tr>
            <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
            <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
            <tr><th/><td><submit action={submit_signup}/></td></tr>
          </table></form></xml>
    end

and signup_page () : transaction page = return <xml><body>{signup_form ()}</body></xml>

and login_form (msgo : option string) : xbody =
    let fun submit_login (login : player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_password_hash login.PassHash
            in  hpo <- oneOrNoRows1 (SELECT player.PassHash
                                     FROM player
                                     WHERE player.Username = {[login.Username]});
                case hpo of
                    None => login_page (Some "Login Failed!")
                  | Some hp =>
                    if hp.PassHash = pw_hs
                    then set_username_cookie login.Username pw_hs;
                         main_menu None
                    else login_page (Some "Login Failed!")
            end
    in  <xml>
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
              </table></form>
          </td></tr></table></xml>
    end

and login_page (msgo : option string) : transaction page = return <xml><body>{login_form msgo}</body></xml>

and new_room_form (pt : player_table) : xbody =
    let fun submit_new_room room_form : transaction page =
            room_id <- nextval room_seq;
            rand <- rand;
            dml (INSERT INTO
                   room (Room, OwnedBy, Nam, Pass, CurrentGame)
                 VALUES
                   ( {[room_id]}
                   , {[pt.Player]}
                   , {[room_form.RoomName]}
                   , {[if room_form.Private then Some rand else None]}
                   , 0 ));
            view_room (Some room_id)
    in  <xml><form>
          <table>
            <tr>New Room</tr>
            <tr><th>Name</th><td><textbox{#RoomName}/></td></tr>
            <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
            <tr><td><submit action={submit_new_room}/></td></tr>
          </table></form></xml>
    end

and new_room () : transaction page = player_page (fn pt => return <xml><body>{new_room_form pt}</body></xml>)

and submit_new_game
        (room : room_table)
        (game_form : game_time_table) : transaction page =
    room_o <-
        oneOrNoRows1 (SELECT *
                      FROM game
                      WHERE game.Room = {[room.Room]}
                        AND game.Game = {[room.CurrentGame]});
    (case room_o of
         Some _ => return ()
       | None   =>
         dml (INSERT INTO game
                ( Game
                , Room
                , ChanNomTime
                , GovVoteTime
                , PresDisTime
                , ChanEnaTime
                , ExecActTime
                , CurrentTurn)
              VALUES
                ( {[room.CurrentGame]}
                , {[room.Room]}
                , {[game_form.ChanNomTime]}
                , {[game_form.GovVoteTime]}
                , {[game_form.PresDisTime]}
                , {[game_form.ChanEnaTime]}
                , {[game_form.ExecActTime]}
                , {[Some 0]})));
    view_room (Some room.Room)

and new_game_form (r : room_table) : xbody =
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
        <tr><td><submit action={submit_new_game r}/></td></tr>
    </table></form></xml>

and choose_room_form (pt : player_table) (room_list : list room_table) : xbody =
    let fun submit_choose_room room_id {} : transaction page =
            let fun debug_and_show_err err =
                    let val frontend_err = "Frontend.new_game: {" ^ pt.Username ^ "}: " ^ err
                    in  debug frontend_err; return <xml>{[frontend_err]}</xml>
                    end
            in  r <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
                m <- oneOrNoRows1 (SELECT *
                                   FROM mod
                                   WHERE mod.Room   = {[room_id]}
                                     AND mod.Player = {[pt.Player]});
                case r of
                    None => debug_and_show_err "Room doesn't exist!"
                  | Some r => if pt.Player <> r.OwnedBy && m = None
                              then debug_and_show_err "You don't own that!"
                              else return <xml><body>{new_game_form r}</body></xml>
            end
    in  List.mapX
            (fn r => <xml><form><submit
                                  value={r.Nam}
                                  action={submit_choose_room r.Room}/></form></xml>)
            room_list
    end

and new_game () : transaction page = player_page
    (fn pt =>
        rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
        case rl of
            [] => new_room ()
          | rl => return <xml><body>New Game<br/>{choose_room_form pt rl}</body></xml>)

and view_room (room_id_o : option int) : transaction page = player_page
    (fn pt =>
        case room_id_o of
            None =>
            rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
            (case rl of
                [] => new_room ()
              | rl =>
                return <xml><body>View Rooms<br/>
                  <table>
                    {List.mapX (fn r =>
                                   <xml><tr><td>
                                     <a link={view_room (Some r.Room)}>{[r.Nam]}</a>
                                   </td></tr></xml>) rl}
                  </table></body></xml>)
          | Some room_id =>
            ro <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
            case ro of
                None    => new_room ()
              | Some rt =>
                room_user_relation_o <-
                    oneOrNoRows1 (SELECT *
                                  FROM room_user
                                  WHERE room_user.Player = {[pt.Player]}
                                    AND room_user.Room   = {[room_id]});
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

and role_page_closure (role : role)
                      (pf : player_table -> transaction page)
    : transaction page =
    check <- Auth.check_login role;
    case check of
        Err err => login_page (Some err)
      | Ok   pt => pf pt

and player_page (pf : player_table -> transaction page) : transaction page =
    role_page_closure Player pf

and admin_page (pf : player_table -> transaction page) : transaction page =
    role_page_closure Admin pf

and admin_test_page () : transaction page = admin_page
    (fn pt =>
        let fun submit_new_player (new_player : string) : transaction page =
                return <xml></xml>
            fun submit_new_room (new_room : string) : transaction page =
                return <xml></xml>
            fun add_user_to_room (user_to_room : string) : transaction page =
                return <xml></xml>
            fun submit_new_game (new_game : string) : transaction page =
                return <xml></xml>
            fun add_user_to_game (is_watching : bool)
                                 (user_to_game : string)
                : transaction page =
                return <xml></xml>
            fun start_game_with_admin_view (game : string) : transaction page =
                return <xml></xml>
        in  return <xml></xml>
        end)

fun game_loop (initial_state : Types.game) =
    let fun loop_it ((me,chan,state) : client * channel action * source Types.game) : transaction {} =
            let fun loop () =
                    sleep 1;
                    loop ()
            in loop () end
    in me <- self;
       chan <- channel;
       (*dml (INSERT INTO users (Client, Chan, Game) VALUES ({[me]}, {[chan]}, {[initial_state.Game]}));*)

       state <- source initial_state;
       loop_it (me,chan,state)
    end
