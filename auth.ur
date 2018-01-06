open Types
open Tables

cookie username_and_pass : player_name_and_pass

fun set_username_cookie u p =
    setCookie username_and_pass
              { Value   = { Username = u
                          , PassHash = p}
              , Expires = None
              , Secure  = False }

datatype role = Admin | Player

val show_role =
    mkShow (fn r => case r of
                        Admin => "admin"
                      | Player => "player")

val admin_list = "sheganinans" :: []

fun is_admin (un : string) : bool = List.exists (fn n => n = un) admin_list

fun check_login (r : role) : transaction (result player_table string) =
    let val err = "Please login."
    in c <- getCookie username_and_pass;
       case c of
           None   => return (Err err)
         | Some c =>
           p_o <- oneOrNoRows1 (SELECT *
                                FROM player
                                WHERE player.Username = {[c.Username]}
                                  AND player.PassHash = {[c.PassHash]});
           case p_o of
               None   => return (Err err)
             | Some p =>
               if p.PassHash = c.PassHash
               then let fun check r b =
                            if not b
                            then return (Err <| "Must be " ^ show r ^ " to do that.")
                            else return (Ok p)
                    in check r (case r of Admin  => is_admin c.Username
                                        | Player => True)
                    end
               else return (Err err)
    end

fun check_role (role : role) : transaction player_table =
    check <- check_login role;
    case check of
        Err (_ : string) => error <xml>(*{login_form {} }*)</xml>
      | Ok   pt          => return pt

fun room_exists_exn (room_id : int) : transaction room_table =
    rt_o <- oneOrNoRows1 (SELECT * FROM room WHERE room.Room = {[room_id]});
    case rt_o of
        None => error <xml>(*{main_menu_body {}}*)</xml>
      | Some rt => return rt

fun player_in_room_exn (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    rp_o <- oneOrNoRows1 (SELECT *
                          FROM room_player
                          WHERE room_player.Room   = {[rt.Room]}
                            AND room_player.Player = {[pt.Player]});
    case rp_o of
        None => error <xml>(*{main_menu_body ()}*)</xml>
      | Some _ => return (pt, rt)

fun only_if_owner_mod_exn (room_id : int) : transaction (player_table * room_table) =
    (pt, rt) <- player_in_room_exn room_id;
    m <- oneOrNoRows1 (SELECT *
                       FROM mod
                       WHERE mod.Room   = {[room_id]}
                         AND mod.Player = {[pt.Player]});
    if rt.OwnedBy = pt.Player || m <> None
    then return (pt, rt)
    else error <xml>(*{main_menu_body {}}*)</xml>

fun only_if_player_not_kicked_exn (room_id : int) : transaction (player_table * room_table) =
    pt <- check_role Player;
    rt <- room_exists_exn room_id;
    is_kicked <- oneOrNoRows1 (SELECT *
                               FROM kick
                               WHERE kick.Player = {[pt.Player]}
                                 AND kick.Room = {[rt.Room]});
    case is_kicked of
        Some _ => error <xml>(*{kicked_body rt}*)</xml>
      | None => return (pt, rt)

fun player_in_game_on_turn_exn (room_id : int)
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


fun basic_hash pw = crypt pw "TheReallyBasicHashSecretForPasswordsForSecretDio"
