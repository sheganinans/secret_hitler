open Types
open Tables

cookie username_and_pass : $player_name_and_pass

fun set_username_cookie (v : $player_name_and_pass) =
    setCookie username_and_pass { Value = v, Expires = None, Secure = False }

datatype role = Admin | Player

val show_role =
    mkShow (fn r => case r of
                        Admin => "admin"
                      | Player => "player")

fun requires_login (redir_page : option string -> transaction page) (r : role) : transaction {} =
    let val err = "You must be logged in to do that."
    in c <- getCookie username_and_pass;
       case c of
           None => redirect (url (redir_page (Some err)))
         | Some c =>
           uo <- oneOrNoRows1 (SELECT player.PassHash
                               FROM player
                               WHERE player.Username = {[c.Username]}
                                 AND player.PassHash = {[c.PassHash]});
           case uo of
               None => redirect (url (redir_page (Some err)))
             | Some u =>
               if u.PassHash = c.PassHash
               then let fun check r b =
                            if not b
                            then redirect (url (redir_page
                                                    (Some <| "Must be " ^ show r ^ "to do that.")))
                            else return ()
                    in check r (case r of Admin =>
                                          List.exists (fn n => n = c.Username) Admin.admin_list
                                        | Player => True)
                    end
               else redirect (url (redir_page (Some err)))
    end
(*
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
