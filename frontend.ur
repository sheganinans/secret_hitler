open Tables
open Auth

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
       loop_it (me,chan,state) end

fun view_room (link : string) =
    return <xml>{[link]}</xml>

fun join_room () : transaction page =
    return <xml>Join Room</xml>

fun main_menu () : transaction page =
    requires_player_login ();
    return <xml><body><table>
      <tr><td><div><a link={new_room ()}>New Room</a></div></td></tr>
      <tr><td><div><a link={new_game ()}>New Game</a></div></td></tr>
      <tr><td>Join Room</td></tr>
    </table></body></xml>

and requires_player_login () = requires_login login_page Player

and signup_page () : transaction page =
    let fun submit_signup (signup : $player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_password_hash signup.PassHash
            in uo <- oneOrNoRows1 (SELECT player.Username
                                   FROM player
                                   WHERE player.Username = {[signup.Username]});

               case uo of
                   Some _ => login_page (Some "Username already taken!")
                 | None =>
                   set_username_cookie { Username = signup.Username, PassHash = pw_hs };

                   player_id <- nextval player_seq;

                   dml (INSERT INTO player (Player, Username, PassHash)
                        VALUES ({[player_id]}, {[signup.Username]}, {[pw_hs]}));

                   main_menu ()
            end
    in return <xml><body><form>
      <table>
        <tr>Signup!</tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_signup}/></td></tr>
      </table></form></body></xml>
    end

and login_page (msgo : option string) : transaction page =
    let fun submit_login (login : $player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_password_hash login.PassHash
            in  hpo <- oneOrNoRows1 (SELECT player.PassHash
                                     FROM player
                                     WHERE player.Username = {[login.Username]});
                case hpo of
                    None => login_page (Some "Login Failed!")
                  | Some hp =>
                    if hp.PassHash = pw_hs
                    then set_username_cookie { Username = login.Username, PassHash = pw_hs };
                         main_menu ()
                    else login_page (Some "Login Failed!")
            end
    in  return <xml><body>
      <a link={signup_page ()}>Need to signup?</a>
      <br/>
      {case msgo of
           None => <xml></xml>
         | Some msg => <xml>{[msg]}<br/></xml>}
      <form>
        <table>
          <tr>Login!</tr>
          <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
          <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
          <tr><th/><td><submit action={submit_login}/></td></tr>
        </table></form></body></xml>
    end

and new_room () : transaction page =
    requires_player_login ();
    let fun submit_new_room room_form : transaction page =
            room_id   <- nextval room_seq;
            let val link = Auth.basic_password_hash (show room_id)
                val pass = if room_form.Private
                           then Some (Auth.basic_password_hash link)
                           else None
            in  with_cookie_or_err
                    "Frontend.new_room"
                    (fn pt =>
                        dml (INSERT INTO room (Room, OwnedBy, Nam, Link, Pass)
                             VALUES ({[room_id]}, {[pt.Player]}, {[room_form.Nam]}, {[link]}, {[pass]}));
                        view_room link)
            end
    in  return <xml><body><form>
          <table>
            <tr>New Room</tr>
            <tr><th>Name</th><td><textbox{#Nam}/></td></tr>
            <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
            <tr><th/><td><submit action={submit_new_room}/></td></tr>
          </table>
        </form></body></xml>
    end

and new_game () : transaction page =
    requires_player_login ();
    with_cookie_or_err
        "Frontend.new_game"
        (fn pt =>
            ro <- queryL1 (SELECT *
                           FROM room
                           WHERE room.OwnedBy = {[pt.Player]});
            return <xml></xml>)

and with_cookie_or_err (err : string) (pf : $player_table -> transaction page)
    : transaction page =
    un_cookie <- getCookie username_and_pass;
    let val err = err ^ ": This should never happen "
    in  case un_cookie of
            None       => login_page (Some <| err ^ "1")
          | Some un_pw =>
            u <- oneOrNoRows1 (SELECT *
                               FROM player
                               WHERE player.Username = {[un_pw.Username]});
            case u of None    => login_page (Some <| err ^ "2")
                    | Some pt => pf pt
    end
