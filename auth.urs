(*val requires_auth : a ::: Type -> b ::: Type ->
                                (string -> (a -> b))
                    -> transaction (option (a -> b))
*)

val basic_password_hash : string -> string
