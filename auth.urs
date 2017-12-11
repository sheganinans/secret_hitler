cookie username_and_pass : $$Types.player_name_and_pass_t

val set_username_cookie : string -> string -> transaction {}

datatype role = Admin | Player

(*val requires_auth : a ::: Type -> b ::: Type ->
                                (string -> (a -> b))
                    -> transaction (option (a -> b))
*)

val admin_list : list string

val check_login : role -> transaction (Types.result
                                           Tables.player_table
                                           string)

val basic_password_hash : string -> string
