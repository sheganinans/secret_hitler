cookie username_and_pass : $$Types.player_name_and_pass

val set_username_cookie : $Types.player_name_and_pass -> transaction {}

datatype role = Admin | Player

(*val requires_auth : a ::: Type -> b ::: Type ->
                                (string -> (a -> b))
                    -> transaction (option (a -> b))
*)

val check_login : role -> transaction (Types.result
                                           { Cookie      : $Types.player_name_and_pass
                                           , PlayerTable : $Tables.player_table }
                                           string)

val basic_password_hash : string -> string
