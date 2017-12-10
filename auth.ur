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

fun check_login (r : role) : transaction (result { Cookie      : $player_name_and_pass
                                                 , PlayerTable : $player_table }
                                                 string) =
    let val err = "You must be logged in to do that."
    in c <- getCookie username_and_pass;
       case c of
           None   => return (Err <| err ^ "No cookie!")
         | Some c =>
           uo <- oneOrNoRows1 (SELECT *
                               FROM player
                               WHERE player.Username = {[c.Username]}
                                 AND player.PassHash = {[c.PassHash]});
           case uo of
               None   => return (Err <| err ^ "No such user!")
             | Some u =>
               if u.PassHash = c.PassHash
               then let fun check r b =
                            if not b
                            then return (Err <| "Must be " ^ show r ^ "to do that.")
                            else return (Ok {Cookie = c, PlayerTable = u})
                    in check r (case r of Admin =>
                                          List.exists (fn n => n = c.Username) Admin.admin_list
                                        | Player => True)
                    end
               else return (Err err)
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
