open Types
open Tables

cookie username_and_pass : player_name_and_pass

fun set_username_cookie u p =
    setCookie username_and_pass { Value   = { Username = u, PassHash = p}
                                , Expires = None
                                , Secure  = False }

datatype role = Admin | Player

val show_role =
    mkShow (fn r => case r of
                        Admin => "admin"
                      | Player => "player")

val admin_list = "sheganinans" :: []

fun is_admin (un : string) : bool = List.exists (fn n => n = un) admin_list

fun basic_hash pw = crypt pw "TheReallyBasicHashSecretForPasswordsForSecretDio"

(* Currently unused *)
signature LESSER_VIEWS = sig
    val     login_form : {} -> xbody
    val main_menu_body : {} -> xbody
    val    kicked_body : room_table -> xbody
end

signature AUTH_SYSTEM = sig
    val check_login : role -> transaction (result player_table string)
    val check_role : xbody -> role -> transaction player_table

    val               room_exists_exn : int -> transaction room_table
    val            player_in_room_exn : int -> transaction (player_table * room_table)
    val         only_if_owner_mod_exn : int -> transaction (player_table * room_table)
    val only_if_player_not_kicked_exn : int -> transaction (player_table * room_table)
    val    player_in_game_on_turn_exn : int -> transaction { Player     :         player_table
                                                           , Room       :           room_table
                                                           , Game       :           game_table
                                                           , Conn       : player_connection
                                                           , TableOrder : table_ordering_table
                                                           , Turn       :           turn_table
                                                           }

    val select_rooms_controlled : {} -> transaction (list room_table)
end

(*
functor AuthSystem(v : LESSER_VIEWS) : AUTH_SYSTEM = struct
end
*)
