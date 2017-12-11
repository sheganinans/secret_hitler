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
