open Auth
open Tables

fun main_menu () : transaction page =
    check <- check_login Player;
    case check of
        Err err => login_page (Some err)
      | Ok _ =>
        return <xml><body><table>
          <tr><td><div><a link={new_room ()}>New Room </a></div></td></tr>
          <tr><td><div><a link={new_game ()}>New Game</a></div></td></tr>
          <tr><td><div><a link={view_room None}>View Rooms</a></div></td></tr>
        </table></body></xml>

and signup_page () : transaction page =
    let fun submit_signup (signup : $player_name_and_pass) : transaction page =
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

                   set_username_cookie { Username = signup.Username, PassHash = pw_hs };

                   main_menu ()
            end
    in  return <xml><body><form>
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

and new_room () : transaction page = with_player_cookie_or_err "Frontend.new_room"
    (fn pt =>
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
        in  return <xml><body><form>
                <table>
                  <tr>New Room</tr>
                  <tr><th>Name</th><td><textbox{#RoomName}/></td></tr>
                  <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
                  <tr><td><submit action={submit_new_room}/></td></tr>
                </table></form></body></xml>
        end)

and new_game () : transaction page = with_player_cookie_or_err "Frontend.new_game"
    (fn pt =>
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
                                  else let fun submit_new_game
                                                   (game_form : $game_time_table) : transaction page =
                                               room_o <-
                                                   oneOrNoRows1 (SELECT *
                                                                 FROM game
                                                                 WHERE game.Room = {[r.Room]}
                                                                   AND game.Game = {[r.CurrentGame]});
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
                                                            ( {[r.CurrentGame]}
                                                            , {[r.Room]}
                                                            , {[game_form.ChanNomTime]}
                                                            , {[game_form.GovVoteTime]}
                                                            , {[game_form.PresDisTime]}
                                                            , {[game_form.ChanEnaTime]}
                                                            , {[game_form.ExecActTime]}
                                                            , {[Some 0]})));
                                               view_room (Some r.Room)
                                       in  return <xml><body><form>
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
                                                 <tr><td><submit
                                                   action={submit_new_game}/></td></tr>
                                               </table>
                                             </form></body></xml>
                                       end
                end
        in  rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
            case rl of
                [] => new_room ()
              | rl =>
                room_list <-
                    List.mapXM
                        (fn r =>
                            return <xml><form>
                              <table>
                                <tr><td>
                                  <submit
                                    value={r.Nam}
                                    action={submit_choose_room r.Room}/></td></tr>
                              </table></form></xml>)
                        rl;
                return <xml><body>New Game<br/>{room_list}</body></xml>
        end)

and view_room (room_id_o : option int)
    : transaction page = with_player_cookie_or_err "Frontend.view_room"
    (fn pt =>
        case room_id_o of
            None =>
            rl <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
            (case rl of
                [] => new_room ()
              | rl =>
                room_list <- List.mapXM (fn r =>
                                            return <xml><tr><td>
                                              <a link={view_room (Some r.Room)}>{[r.Nam]}</a>
                                            </td></tr></xml>) rl;
                return <xml><body>View Rooms<br/>
                  <table>{room_list}</table></body></xml>)
          | Some room_id =>
            ro <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
            case ro of
                None => new_room ()
              | Some rt =>
                room_user_relation_o <-
                    oneOrNoRows1 (SELECT *
                                  FROM room_user
                                  WHERE room_user.Player = {[pt.Player]}
                                    AND room_user.Room   = {[room_id]});
                case room_user_relation_o of
                    None => join_room room_id
                  | Some _ =>
                    is_banned <-
                        oneOrNoRows1 (SELECT *
                                      FROM ban
                                      WHERE ban.Player = {[pt.Player]}
                                        AND ban.Room   = {[room_id]});
                    case is_banned of
                        Some _ => banned_page room_id
                      | None => return <xml></xml>)

and join_room room_id : transaction page = return <xml></xml>

and banned_page (room_id : int) : transaction page = return <xml>Banned!</xml>

and with_player_cookie_or_err (err : string)
                                (pf : $player_table -> transaction page) : transaction page =
    check <- Auth.check_login Player;
    let val err = err ^ ": This should never happen: "
        fun debug_and_redir_to_login inner_err =
            let val frontend_err_msg = err ^ inner_err
            in  debug frontend_err_msg; login_page (Some frontend_err_msg)
            end
    in  case check of
            Err err => debug_and_redir_to_login err
          | Ok ret => pf ret.PlayerTable
    end

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
