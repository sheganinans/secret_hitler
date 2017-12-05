(*
datatype role = Admin | Player

val show_role =
    mkShow (fn r => case r of
                        Admin => "admin"
                      | Player => "player")

fun require_login (r : role) : transaction {} =
    let val err = "You must be logged in to do that."
    in c <- getCookie username_and_pass;
       case c of
           None => error <xml>{[err]}</xml>
         | Some c =>
           u <- oneOrNoRows1 (SELECT player.PassHash
                              FROM player
                              WHERE player.Nam = {[c.Nam]});
           case u of
               None => error <xml>{[err]}</xml>
             | Some u =>
               if u.PassHash = Some c.PassHash
               then let fun check b r = if b
                                        then return ()
                                        else error <xml>Must be {[show r]} to do that.</xml>
                    in check (case r of Admin  => List.exists (fn n => n = c.Nam) Admin.admin_list
                                      | Player => True) r
                    end
               else error <xml>{[err]}</xml>
    end

fun requires_auth [a ::: Type] [b ::: Type]
      (f : string ->      (a -> b))
    : transaction (option (a -> b)) =
    (* TODO: Actually make work and replace basic_password_hash. *)
    vo <- getenv (blessEnvVar "PASSWORD_SECRET");
    case vo of
        None   => debug "Auth.requires_auth, No ENV_VAR: PASSWORD_SECRET";
                  return None
      | Some v => return <| Some <| f v
*)

fun basic_password_hash pw = crypt pw "TheReallyBasicHashSecretForPasswordsForSecretDio"
