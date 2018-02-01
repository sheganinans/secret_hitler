structure B = Bootstrap3

open Auth
open Protocol
open Tables
open Utils

fun on_view_room_load (pt : player_table) (gt : game_table) : transaction {} =
    debug "ovrl";
    rule_set <- oneRow1 (SELECT *
                         FROM rule_set
                         WHERE rule_set.Room = {[gt.Room]}
                           AND rule_set.Game = {[gt.Game]});

    chan <- oneRow1 (SELECT player_in_game.Chan
                     FROM player_in_game
                     WHERE player_in_game.Room = {[gt.Room]}
                       AND player_in_game.Game = {[gt.Game]}
                       AND player_in_game.Player = {[pt.Player]});
    room_table <- oneRow1 (SELECT *
                           FROM room
                           WHERE room.Room = {[gt.Room]});

    curr_players <- get_players_playing gt;

    mapM_ (fn m => send chan.Chan (PublicRsp m))
              (PlayersOnTable curr_players          ::
               RuleSet (rule_set -- #Room -- #Game) ::
               []);

    if not room_table.InGame
    then
        curr_players <- get_players_playing gt;

        mapM_ (fn m => send chan.Chan (PublicRsp m))
              (PlayersOnTable curr_players          ::
               RuleSet (rule_set -- #Room -- #Game) ::
               [])
    else
        step <- oneRow1 (SELECT turn.CurrentStep
                         FROM turn
                         WHERE turn.Room = {[gt.Room]}
                           AND turn.Game = {[gt.Game]}
                           AND turn.Turn = {[gt.CurrentTurn]});

        curr_govt_state <- oneRow1 (SELECT turn.President
                                      , turn.Chancellor
                                      , turn.LiberalPolicies
                                      , turn.FascistPolicies
                                      , turn.RejectCount
                                    FROM turn
                                    WHERE turn.Room = {[gt.Room]}
                                      AND turn.Game = {[gt.Game]}
                                      AND turn.Turn = {[gt.CurrentTurn]});

        govt_history <- queryL1 (SELECT turn.President
                                   , turn.Chancellor
                                   , turn.LiberalPolicies
                                   , turn.FascistPolicies
                                   , turn.RejectCount
                                 FROM turn
                                 WHERE turn.Room = {[gt.Room]}
                                   AND turn.Game = {[gt.Game]}
                                   AND turn.Turn <> {[gt.CurrentTurn]}
                                 ORDER BY turn.Turn);

        send chan.Chan (PublicRsp (PublicGameState
                                       { CurrentStep = deserialize step.CurrentStep
                                       , CurrentTurn = curr_govt_state
                                       , GameHistory = govt_history
                                       , Players = []
                       }))

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
                    redirect (url (main_menu {}))
            end

    in  return <xml><head>{Head.std_head}</head>
      <body><form><table>
        <tr><th>Signup</th></tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_signup}/></td></tr>
    </table></form></body></xml>
    end

and check_role r = check_role_closure login_form r

and room_exists_exn (room_id : int) : transaction room_table =
    rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case rt_o of
        None => error <xml>(*{LV.main_menu_body {}}*)</xml>
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

and login_page {} : transaction page =
    return <xml><head>{Head.std_head}</head>
      <body>{login_form {}}</body></xml>

and login_form {} : make_form = <xml><table>
  <tr><td><a link={main_menu {}}>Back to Main Menu</a></td></tr>
  <tr><td><a link={signup_page {}}>Need to Signup?</a></td></tr>
  <tr><td>
    <form>
      <table>
        <tr>Login!</tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit class={cl (B.btn :: B.btn_default :: [])}
                             action={submit_login}/></td></tr>
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
            then redirect (url (login_page {}))
            else set_username_cookie login_form.Username pw_hs;
                 redirect (url (main_menu {}))
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
    return <xml><head>{Head.std_head}</head><body>{kicked_body rt}</body></xml>

and main_menu {} : transaction page =
    pt <- check_role Player;
    return <xml><head>{Head.std_head}</head><body>{main_menu_body {}}</body></xml>

and main_menu_button {} : xbody = <xml>
  <button class={cl (B.btn :: B.btn_primary :: [])}
          style="width:100%"
          onclick={fn _ => redirect (url (main_menu {}))}>Main Menu</button></xml>

and redir_button (page : transaction page) (v : string) : xbody =
    <xml><button class={cl (B.btn :: B.btn_default :: [])}
                onclick={fn _ => redirect (url (page))}>{[v]}</button></xml>

and main_menu_body {} : xbody =
    <xml>
      <div class={B.btn_group_vertical} role="group" style="width:100%">
        {redir_button (view_public_rooms {}) "View Public Rooms"}
        {redir_button (new_game          {}) "New Game"}
        {redir_button (view_your_rooms   {}) "View Your Rooms"}
        {redir_button (new_room          {}) "New Room"}
    </div></xml>

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
            redirect (url (view_room room_id))

    in  return <xml><head>{Head.std_head}</head><body>
      {main_menu_button {}}
      <form>
        <table>
          <tr><th>New Room</th></tr>
          <tr><th>Name</th><td><textbox{#RoomName}/></td></tr>
          <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
          <tr><td><submit class={cl (B.btn :: B.btn_default :: [])}
                          value="New Room"
                          action={submit_new_room}/></td></tr>
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
    redirect (url (view_room rt.Room))

and new_game_page (room_id : int) : transaction page =
    return <xml><head>{Head.std_head}</head><body><form><table>
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
    return <xml><head>{Head.std_head}</head><body>
      <div class={B.btn_group_vertical} role="group" style="width:100%">
        {main_menu_button {}}
        {List.mapX (fn r => <xml> {redir_button (new_game_page r.Room) r.Nam}</xml>)
                   (List.filter (fn r => not r.InGame) room_list)}
    </div></body></xml>

and view_your_rooms {} : transaction page =
    rl <- select_rooms_controlled {};
    case rl of
        [] => main_menu {}
      | rl =>
        return <xml><head>{Head.std_head}</head><body>
          <div class={B.btn_group_vertical} role="group" style="width:100%">
            {main_menu_button {}}
            (* TODO truncate long names *)
            {List.mapX (fn r => <xml>{redir_button (view_room r.Room) r.Nam}</xml>) rl}
        </div></body></xml>

and view_public_rooms {} : transaction page =
    rl <- queryL1 (SELECT * FROM room WHERE room.Pass = NULL);
    return <xml><head>{Head.std_head}</head><body>
      <div class={B.btn_group_vertical} role="group" style="width:100%">
        {main_menu_button {}}
        {List.mapX (fn r => <xml>{redir_button (join_room r.Room) r.Nam}</xml>) rl}
    </div></body></xml>

and view_invite (room_id : int) : transaction page =
    (pt, rt) <- player_in_room_exn room_id;
    return <xml><head>{Head.std_head}</head><body><table>
      <tr><th>{redir_button (view_room room_id) ("Back to " ^ rt.Nam)}</th></tr>
      <tr><th>Link:</th><td>
        {redir_button (view_room room_id)
                      (Consts.website_url ^ show (url (view_room room_id)))}</td></tr>
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
            redirect (url (view_room room_id))

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
                return <xml><head>{Head.std_head}</head><body><form><table>
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
                 add_player_to_game rt pt me chan playing num_in_game;
                 send_public_message gt (NewPlayer { Username = pt.Username
                                                   , InGameId = num_in_game }));
            redirect (url (view_room room_id))

    in  if rt.InGame
        then submit_join_game False {}
        else return <xml><head>{Head.std_head}</head><body>
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
        let fun no_game_yet {} : transaction xbody = return <xml>No Game Yet</xml>

        in  mods <- queryL1 (SELECT * FROM mod WHERE mod.Room = {[rt.Room]});

            curr_game <- oneOrNoRows1 (SELECT *
                                       FROM game
                                       WHERE game.Room = {[rt.Room]}
                                         AND game.Game = {[rt.CurrentGame]});
            case curr_game of
                None => if rt.OwnedBy = pt.Player ||
                           List.exists (fn m => pt.Player = m.Player) mods
                        then new_game_page room_id
                        else return <xml><body>
                          <a link={view_invite room_id}>View Invite</a>
                          <br/>No Game yet</body></xml>
              | Some gt =>
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
                    (page_s : source xbody) <- source (<xml></xml>);
                    let val handler = Game_view.handler pt rt gt page_s
                    in
                        return <xml><head>
                          {Head.std_head}
                          <script code={let fun rsp_loop {} =
                                                msg <- recv chan;
                                                handler msg;
                                                rsp_loop {}
                                        in spawn (rsp_loop {}) end}/></head>

                          <body onload={rpc (on_view_room_load pt gt)}>
                            <table style="width:100%">
                              <tr><td>
                                <div class={B.btn_group_vertical} role="group" style="width:100%">
                                  {main_menu_button {}}
                                  {redir_button (view_invite room_id) "View Invite"}
                              </div></td></tr>
                              <tr><td><dyn signal={signal page_s}/></td></tr></table></body></xml>
                    end
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
            redirect (url (view_room room_id))

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
            redirect (url (view_room room_id))

    in  player_list <- get_all_un_and_id_in_room room_id;
        return <xml><head>{Head.std_head}</head><body>
          {List.mapX (fn p =>
                         <xml><form>
                           <submit value={p.Username}
                                   action={submit_rem_mod room_id p.Player}/>
                         </form></xml>)
                     player_list}</body></xml>
    end
