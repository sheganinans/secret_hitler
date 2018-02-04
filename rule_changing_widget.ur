structure B = Bootstrap3

open Tables
open Types

fun make (pt : player_table)
         (rt :   room_table)
         (gt :   game_table)
         (ss : source rule_set)
    : transaction { View : xbody
                  , Modal : xbody
                  , Button : xbody
                  } =

    kill_p <- source False;
    timed_g <- source False;
    cn_time <- source (Some 0.);
    gv_time <- source (Some 0.);
    pd_time <- source (Some 0.);
    ce_time <- source (Some 0.);
    ea_time <- source (Some 0.);

    change_rules_id <- fresh;
    mods <- get_mods rt;

    let fun submit_new_rules r : transaction {} =
            dml (DELETE FROM rule_set
                        WHERE Room = {[rt.Room]}
                          AND Game = {[rt.CurrentGame]});
            set_rules rt r;
            send_public_message gt (RuleSet r)

        fun update_rules (rs : rule_set) : transaction {} =
            set kill_p rs.KillPlayer;
            set timed_g rs.TimedGame;
            if not rs.TimedGame
            then return {}
            else set cn_time (Some rs.ChanNomTime);
                 set gv_time (Some rs.GovVoteTime);
                 set pd_time (Some rs.PresDisTime);
                 set ce_time (Some rs.ChanEnaTime);
                 set ea_time (Some rs.ExecActTime)

        fun z_if_n (o : option float) : float = case o of
                                                    None => 0.
                                                  | Some f => f

        fun capture_rules {} : transaction rule_set =
            kill_p  <- get kill_p;
            timed_g <- get timed_g;
            cn_time <- get cn_time;
            gv_time <- get gv_time;
            pd_time <- get pd_time;
            ce_time <- get ce_time;
            ea_time <- get ea_time;
            return { KillPlayer = kill_p
                   , TimedGame = timed_g
                   , ChanNomTime = z_if_n cn_time
                   , GovVoteTime = z_if_n gv_time
                   , PresDisTime = z_if_n pd_time
                   , ChanEnaTime = z_if_n ce_time
                   , ExecActTime = z_if_n ea_time
                   }

    in return { View = <xml>
      <dyn signal={
                ss <- signal ss;
                return <xml>
                  <table>
                    <tr><th>{[(if ss.KillPlayer
                               then "Player killed"
                               else "Turn skipped") ^ " as punishment."]}</th></tr>
                    {if not ss.TimedGame
                     then <xml><tr><th>Game untimed</th></tr></xml>
                     else <xml>
                       <tr><th>Chancellor Nomination</th>
                         <td>{[ss.ChanNomTime]}</td></tr>
                       <tr><th>Governmant Vote</th>
                         <td>{[ss.GovVoteTime]}</td></tr>
                       <tr><th>President Discard</th>
                         <td>{[ss.PresDisTime]}</td></tr>
                       <tr><th>Chancellor Enaction</th>
                         <td>{[ss.ChanEnaTime]}</td></tr>
                       <tr><th>Executive Action</th>
                         <td>{[ss.ExecActTime]}</td></tr>
                       </xml>}</table></xml>}/></xml>
              , Modal = <xml>
                <div class={cl (B.modal :: B.fade :: [])} id={change_rules_id} role="dialog">
                  <div class={B.modal_dialog} role="document">
                    <div class={B.modal_content}>
                      <div class={B.modal_header}>
                        <button class={B.close}
                                onclick={fn _ => return {} (*reset_rules {}*)}
                                data-dismiss="modal" aria-label="Close">
                          <span aria-hidden="true">&times;</span></button>
                          <h4 class={B.modal_title}>Change Rules</h4>
                      </div>
                      <div class={B.modal_body}>
                        <table>
                          <tr><th>Kill Player as Punishment?</th>
                            <td><ccheckbox source={kill_p}/></td></tr>
                          <tr><th>Timed Game?</th>
                            <td><ccheckbox source={timed_g}/></td></tr>
                          <tr><th>Chancellor Nomination</th>
                            <td><cnumber source={cn_time}/></td></tr>
                          <tr><th>Government Vote</th>
                            <td><cnumber source={gv_time}/></td></tr>
                          <tr><th>President Discard</th>
                            <td><cnumber source={pd_time}/></td></tr>
                          <tr><th>Chancellor Enaction</th>
                            <td><cnumber source={ce_time}/></td></tr>
                          <tr><th>Executive Action</th>
                            <td><cnumber source={ea_time}/></td></tr>
                  </table>
                      </div>
                      <div class={B.modal_footer}>
                        <button class={cl (B.btn :: B.btn_default :: [])}
                                data-dismiss="modal">Close</button>
                        <button class={cl (B.btn :: B.btn_primary :: [])}
                                data-dismiss="modal"
                                value="Change it!"
                                onclick={fn _ =>
                                            rs <- capture_rules {};
                                            update_rules rs;
                                            rpc (submit_new_rules rs)
                                        }/>
                 </div></div></div></div></xml>
               , Button = <xml>
                 {if rt.OwnedBy <> pt.Player &&
                     not (List.exists (fn m => pt.Player = m.Player) mods)
                  then <xml></xml>
                  else <xml><button class={cl (B.btn :: B.btn_primary :: [])}
                                    onclick={fn _ =>
                                                rs <- get ss;
                                                update_rules rs}
                                    data-toggle="modal"
                                    data-target={"#" ^ show change_rules_id}>Change Rules
                            </button></xml>}</xml>
              }
    end
