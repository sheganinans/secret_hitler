open Auth
open Protocol
open Tables
open Utils

fun signup_page {} : transaction page =
    let fun submit_signup (signup : player_name_and_pass) : transaction page =
            let val pw_hs = Auth.basic_hash signup.PassHash
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

and ban_player (room_id : int) : transaction page = return <xml></xml>

and banned_body (rt : room_table) : xbody =
    <xml><table>
      <tr><th>You have been banned from room: {[rt.Nam]}</th></tr>
      <tr>Appeal system coming soon!</tr>
      <tr><td><a link={main_menu {}}>Main Menu</a></td></tr>
    </table></xml>

and banned (room_id : int) : transaction page =
    (_, rt) <- player_in_room_exn room_id;
    return <xml><body>{banned_body rt}</body></xml>

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

and new_game {} : transaction page =
    let fun submit_new_game (room_id: int) (rule_set : rule_set) : transaction page =
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
                       ( Game
                       , Room
                       , TimedGame
                       , KillPlayer
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
                   , {[rule_set.TimedGame]}
                   , {[rule_set.KillPlayer]}
                   , {[rule_set.ChanNomTime]}
                   , {[rule_set.GovVoteTime]}
                   , {[rule_set.PresDisTime]}
                   , {[rule_set.ChanEnaTime]}
                   , {[rule_set.ExecActTime]}
                   , {[0]}
                   , {[now]}
                   , {[now]}
                   , {[None]} )));
            view_room rt.Room

        fun new_game_page (room_id : int) {} : transaction page =
            return <xml><body><form>
              <table>
                <tr><th>New Game: Time Table</th></tr>
                <tr><th>Timed Game?</th>
                  <td><checkbox{#TimedGame}/></td></tr>
                <tr><th>Kill Player as Punishment?</th>
                  <td><checkbox{#KillPlayer}/></td></tr>
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
            {List.mapX (fn r =>
                           <xml><tr><td>
                             <a link={view_room r.Room}>
                               {[r.Nam]}</a></td></tr></xml>)
                       rl}
          </table></body></xml>

and view_public_rooms {} : transaction page =
    rl <- queryL1 (SELECT * FROM room WHERE room.Pass = NULL);
    return <xml><body><table>
      <tr><td><a link={main_menu {}}>Main Menu</a></td></tr>
      {List.mapX (fn r =>
                     <xml><tr><td>
                       <a link={join_room r.Room}>
                         {[show r.Nam]}</a></td></tr></xml>)
                 rl}
    </table></body></xml>

and view_invite (room_id : int) : transaction page =
    (pt, rt) <- player_in_room_exn room_id;
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

and join_game (room_id : int) : transaction page =
    let fun submit_join_game (room_id : int)
                             (playing : { Playing : bool }) : transaction page =
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
                       , Watching
                       , InGameId )
                     VALUES
                       ( {[rt.CurrentGame]}
                       , {[rt.Room]}
                       , {[pt.Player]}
                       , {[me]}
                       , {[chan]}
                       , {[if rt.InGame then True else not playing.Playing]}
                       , NULL )));
            view_room room_id

    in  (pt, rt) <- player_in_room_exn room_id;
        if rt.InGame
        then submit_join_game room_id {Playing = False}
        else return <xml><body><form><table>
                      <tr><th>Playing?</th><td><checkbox{#Playing}/></td></tr>
                      <tr><submit action={submit_join_game room_id}/></tr>
                    </table></form></body></xml>
    end

and view_room (room_id : int) : transaction page =
    (pt, rt) <- only_if_player_not_banned_or_kicked_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Player = {[pt.Player]}
                            AND room_player.Room   = {[rt.Room]});
    case rp_o of
        None   => join_room room_id
      | Some _ =>
        let fun no_game_yet {} : transaction xbody = return <xml></xml>

            fun fascist_view {} : transaction xbody = return <xml></xml>

            fun hitler_view {} : transaction xbody = return <xml></xml>

            fun liberal_view {} : transaction xbody = return <xml></xml>

            fun watcher_view {} : transaction xbody = return <xml></xml>

        in  player_list <- get_all_un_and_id_in_room room_id;
            return <xml><body onload={return {}}><table>
              <tr><th>View Room {[show rt.Nam]}</th></tr>
              {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                         player_list}
            </table></body></xml>
        end

and start_game (room_id : int) : transaction page =
    (pt, rt) <- only_if_owner_mod_exn room_id;
    player_list <- queryL1 (SELECT *
                            FROM player_in_game
                            WHERE player_in_game.Game = {[rt.CurrentGame]}
                              AND player_in_game.Room = {[rt.Room]});
    players <- Utils.shuffle (List.filter (fn p => not p.Watching) player_list);
    let val players_l = List.length players
        val get_pid = get_player_from_in_game_id rt
    in  (if players_l < 5 || players_l > 10
         then return {}
         else case List.find (fn (k,_) => k = players_l) player_numbers_table of
                  None    => return {}
                | Some (_, lf) =>
                  mapM_ (fn (i,p) =>
                            dml (UPDATE player_in_game
                                 SET InGameId = {[Some i]}
                                 WHERE Room = {[rt.Room]}
                                   AND Game = {[rt.CurrentGame]}
                                   AND Player = {[p.Player]} ))
                        (List.mapi (fn i x => (i,x)) players);

                  mapM_ (fn l =>
                            dml (INSERT INTO liberal (Game, Room, Player)
                                 VALUES
                                   ( {[rt.CurrentGame]}
                                   , {[rt.Room]}
                                   , {[l.Player]} )))
                        (List.take lf.Liberals players);

                  dml (INSERT INTO hitler (Game, Room, Player)
                       VALUES
                         ( {[rt.CurrentGame]}
                         , {[rt.Room]}
                         , {[(List.nth players lf.Liberals |> Option.unsafeGet).Player]} ));

                  mapM_ (fn f =>
                            dml (INSERT INTO fascist (Game, Room, Player)
                                 VALUES
                                   ( {[rt.CurrentGame]}
                                   , {[rt.Room]}
                                   , {[f.Player]} )))
                        (List.drop (lf.Liberals + 1) players);

                  pres     <- get_pid 0;
                  nextPres <- get_pid 1;
                  dml (INSERT INTO turn
                         ( Game
                         , Room
                         , Turn
                         , President
                         , NextPres
                         , Chancellor
                         , RejectCount
                         , VoteDone
                         , LiberalsInDraw
                         , FascistsInDraw
                         , LiberalsInDisc
                         , FascistsInDisc
                         , Fst
                         , Snd
                         , Trd
                         , PresDisc
                         , ChanEnac
                         , LiberalPolicies
                         , FascistPolicies )
                       VALUES
                         ( {[rt.CurrentGame]}
                         , {[rt.Room]}
                         , 1
                         , {[pres.Player]}
                         , {[nextPres.Player]}
                         , NULL
                         , 0
                         , FALSE
                         , {[number_of_liberal_policies]}
                         , {[number_of_fascist_policies]}
                         , 0
                         , 0
                         , {[True]} (* TODO *)
                         , {[True]}
                         , {[True]}
                         , 0
                         , 0
                         , 0
                         , 0 ));

                  now <- now;
                  dml (UPDATE game
                       SET LastAction = {[now]}
                       WHERE Room = {[rt.Room]}
                         AND Game = {[rt.CurrentGame]}));
        view_room room_id
    end

and get_all_un_and_id_in_room (room_id : int) : transaction (list player_id_and_username) =
    _ <- player_in_room_exn room_id;
    queryL1 (SELECT player.Player, player.Username
             FROM (room_player
                 INNER JOIN player
                 ON room_player.Player = player.Player)
             WHERE room_player.Room = {[room_id]})

and new_mod {} : transaction page =
    let fun submit_new_mod (room_id : int) (player_id : int) {} =
            (pt, rt) <- only_if_owner_mod_exn room_id;
            now <- now;
            dml (INSERT INTO mod (Room, Player, SetBy, Time)
                 VALUES ({[rt.Room]}, {[player_id]}, {[pt.Player]}, {[now]}));
            view_room room_id

        fun new_mod_page (room_id : int) {} : transaction page =
            player_list <- get_all_un_and_id_in_room room_id;
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

and rem_mod {} : transaction page = return <xml></xml>

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
                          WHERE room_player.Room   = {[rt.Room]}
                            AND room_player.Player = {[pt.Player]});
    case rp_o of
        None => error <xml>{main_menu_body ()}</xml>
      | Some _ => return (pt, rt)

and player_in_game_exn (room_id : int)
    : transaction (player_table * room_table * game_table * turn_table) =
    (pt, rt) <- player_in_room_exn room_id;
    gt_o <- oneOrNoRows1 (SELECT *
                          FROM game
                          WHERE game.Room = {[rt.Room]}
                            AND game.Game = {[rt.CurrentGame]});
    case gt_o of
        None => error <xml>{main_menu_body ()}</xml>
      | Some gt =>
        tt_o <- oneOrNoRows1 (SELECT *
                              FROM turn
                              WHERE turn.Room = {[rt.Room]}
                                AND turn.Game = {[rt.CurrentGame]}
                                AND turn.Turn = {[gt.CurrentTurn]});
        case tt_o of
            None => error <xml>{main_menu_body ()}</xml>
          | Some tt => return (pt, rt, gt, tt)

and only_if_owner_mod_exn (room_id : int) : transaction (player_table * room_table) =
    (pt, rt) <- player_in_room_exn room_id;
    m <- oneOrNoRows1 (SELECT *
                       FROM mod
                       WHERE mod.Room   = {[room_id]}
                         AND mod.Player = {[pt.Player]});
    if rt.OwnedBy = pt.Player || m <> None
    then return (pt, rt)
    else error <xml>{main_menu_body {}}</xml>

and only_if_player_not_banned_or_kicked_exn (room_id : int)
    : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    is_banned <- oneOrNoRows1 (SELECT *
                               FROM ban
                               WHERE ban.Player = {[pt.Player]}
                                 AND ban.Room   = {[rt.Room]});
    case is_banned of
        Some _ => error <xml>{banned_body rt}</xml>
      | None   =>
        is_kicked <- oneOrNoRows1 (SELECT *
                                   FROM kick
                                   WHERE kick.Player = {[pt.Player]}
                                     AND kick.Room = {[rt.Room]});
        case is_kicked of
            Some _ => error <xml>{banned_body rt}</xml>
          | None => return (pt, rt)

and if_player_in_good_standing (room_id : int)
                               (page_f : player_table * room_table -> transaction page)
    : transaction page =
    (pt, rt) <- only_if_player_not_banned_or_kicked_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Player = {[pt.Player]}
                            AND room_player.Room   = {[rt.Room]});
    case rp_o of
        None   => join_room room_id
      | Some _ => page_f (pt, rt)

and select_rooms_controlled {} : transaction (list room_table) =
    pt <- check_role Player;
    rl_1 <- queryL1 (SELECT * FROM room WHERE room.OwnedBy = {[pt.Player]});
    rl_2 <- queryL1 (SELECT room.*
                     FROM (room
                         INNER JOIN mod
                         ON room.Room   = mod.Room
                         AND mod.Player = {[pt.Player]}));
    return (List.append rl_1 rl_2)

fun players_in_game (gt : game_table) : transaction (list player_connection) =
    queryL1 (SELECT *
             FROM player_in_game
             WHERE player_in_game.Game = {[gt.Game]}
               AND player_in_game.Room = {[gt.Room]})

fun current_turn_state (gt : game_table) : transaction turn_table =
    oneRow1 (SELECT *
             FROM turn
             WHERE turn.Game = {[gt.Game]}
               AND turn.Room = {[gt.Room]}
               AND turn.Turn = {[gt.CurrentTurn]})

fun update_last_action (gt : game_table) =
    now <- now;
    dml (UPDATE game
         SET LastAction = {[now]}
         WHERE Game = {[gt.Game]}
           AND Room = {[gt.Room]})


fun player_action (room_id : int) (action : Protocol.in_game) : transaction {} =
    (pt, rt, gt, tt) <- player_in_game_exn room_id;
    case action of
      VoterAction      _ => return {}
    | PresidentAction  _ => return {}
    | ChancellorAction _ => return {}

fun skip_turn (gt : game_table) : transaction {} = return {}

fun enact_skip_turn_or_kill (gt : game_table)
                            (kill_f : turn_table -> transaction {}) : transaction {} =
    if gt.KillPlayer
    then turn <- current_turn_state gt; kill_f turn
    else skip_turn gt

fun punish_president (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn turn =>
        dml (INSERT INTO dead_player (Game, Room, Turn, Target)
             VALUES ( {[turn.Game]}
                    , {[turn.Room]}
                    , {[turn.Turn]}
                    , {[turn.President]} )))

fun punish_chancellor (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn turn =>
        dml (INSERT INTO dead_player (Game, Room, Turn, Target)
             VALUES ( {[turn.Game]}
                    , {[turn.Room]}
                    , {[turn.Turn]}
                    , {[Option.unsafeGet turn.Chancellor]} )))

fun punish_non_voters (gt : game_table) : transaction {} =
    enact_skip_turn_or_kill gt (fn turn =>
        player_list <- queryL1 (SELECT player_in_game.InGameId
                                FROM (player_in_game
                                    LEFT JOIN vote_on_govt
                                    ON vote_on_govt.Player = player_in_game.Player)
                                WHERE player_in_game.Watching = FALSE
                                  AND player_in_game.Game = {[gt.Game]}
                                  AND player_in_game.Room = {[gt.Room]});

        mapM_ (fn p =>
                  dml (INSERT INTO dead_player (Game, Room, Turn, Target)
                       VALUES ( {[turn.Game]}
                              , {[turn.Room]}
                              , {[turn.Turn]}
                              , {[Option.unsafeGet p.InGameId]} )))
              player_list)


fun player_chans_in_game (gt : game_table)
    : transaction (list { Chan : channel in_game_response }) =
    queryL1 (SELECT player_in_game.Chan
             FROM player_in_game
             WHERE player_in_game.Room = {[gt.Room]}
               AND player_in_game.Game = {[gt.Game]})

fun vote_failed (gt : game_table) : transaction {} =
    players_chans <- player_chans_in_game gt;
    turn <- current_turn_state gt;
    let val state = if turn.RejectCount > 3 then InChaos else Failed
    in  (case state of
             Passed => return {(* Will never happen *)}
           | InChaos => return {}
           | Failed  => return {});
        mapM_ (fn p => send p.Chan (GeneralRsp (NewGovt state))) players_chans
    end

fun liberals_win (gt : game_table) : transaction {} = return {}

fun fascists_win (gt : game_table) : transaction {} = return {}


fun enact_veto (gt : game_table) : transaction {} = return {}

fun enact_policy (gt : game_table) : transaction {} =
    let fun investigate_loyalty (gt : game_table) : transaction {} = return {}

        fun call_special_election (gt : game_table) : transaction {} = return {}

        fun policy_peek (gt : game_table) : transaction {} = return {}

        fun execution (gt : game_table) : transaction {} = return {}

        fun veto_power (gt : game_table) : transaction {} = return {}

    in  (* Enacting a policy may change the number of fascist policies. *)
        turn <- current_turn_state gt;
        (case turn.FascistPolicies of
             0 => fn _ => return {}
           | 1 => investigate_loyalty
           | 2 => call_special_election
           | 3 => policy_peek
           | 4 => execution
           | 5 => veto_power
           | _ => fascists_win) gt
    end

fun game_loop (gt : game_table) =
    turn <- current_turn_state gt;
    now <- now;
    let fun action_not_overdue (delta : float) : bool =
            if gt.TimedGame
            then addSeconds gt.LastAction (ceil (delta * 60.)) > now
            else True
    in  case turn.LiberalPolicies of
            5 => liberals_win gt
          | _ =>
            case turn.FascistPolicies of
                6 => fascists_win gt
              | _ =>
                case turn.Chancellor of
                    None =>
                    if action_not_overdue gt.ChanNomTime
                    then return {}
                    else punish_president gt
                  | Some _ =>
                    players <- players_in_game gt;
                    votes <- queryL1 (SELECT *
                                      FROM vote_on_govt
                                      WHERE vote_on_govt.Game = {[gt.Game]}
                                        AND vote_on_govt.Room = {[gt.Room]}
                                        AND vote_on_govt.Turn = {[turn.Turn]});
                    if List.length votes <> List.length players
                    then if action_not_overdue gt.GovVoteTime
                         then return {}
                         else punish_non_voters gt
                    else
                        let val yes = List.filter (fn v =>     v.Vote) votes
                            val nos = List.filter (fn v => not v.Vote) votes
                        in  if List.length nos >= List.length yes
                            then vote_failed gt
                            else (* New govt *)
                                (if turn.FascistPolicies <= 3
                                 then return {}
                                 else
                                     hitler_o <- oneOrNoRows1 (SELECT *
                                                               FROM hitler
                                                               WHERE hitler.Game = {[gt.Game]}
                                                                 AND hitler.Room = {[gt.Room]}
                                                                 AND hitler.Player =
                                                                       {[Option.unsafeGet
                                                                           turn.Chancellor]});
                                     case hitler_o of
                                         None   => return {}
                                       | Some _ => fascists_win gt);
                                case turn.PresDisc of
                                    0 => if action_not_overdue gt.PresDisTime
                                         then return {}
                                         else punish_president gt
                                  | _ =>
                                    veto_o <- oneOrNoRows1 (SELECT *
                                                            FROM veto
                                                            WHERE veto.Game = {[gt.Game]}
                                                              AND veto.Room = {[gt.Room]}
                                                              AND veto.Turn = {[turn.Turn]});
                                    case veto_o of
                                        Some _ => enact_veto gt
                                      | None =>
                                        case turn.ChanEnac of
                                            0 => if action_not_overdue gt.ChanEnaTime
                                                 then return {}
                                                 else punish_chancellor gt
                                          | p => enact_policy gt
                        end
    end

task periodic 1 = (* Loop for active games. *)
     fn {} =>
        games <- queryL1 (SELECT game.*
                          FROM (game
                              INNER JOIN room
                              ON  game.Room = room.Room
                              AND game.Game = room.CurrentGame));
        mapM_ game_loop games;
        return {}

task periodic 60 = (* Kick removal loop *)
     fn {} =>
        now <- now;
        dml (DELETE FROM kick WHERE Till < {[now]})

fun admin_view {} : transaction page =
    let fun submit_admin_view {} : transaction page = return <xml></xml>
    in  pt <- check_role Admin;
        return <xml></xml>
    end
