structure B = Bootstrap3

open Tables
open Types

fun rule_changing_widget (rt : room_table)
                         (gt : game_table)
    : transaction { Modal : xbody
                  , Button: xbody
                  , Sources : { KillPlayer  : source bool
                              , TimedGame   : source bool
                              , ChanNomTime : source (option float)
                              , GovVoteTime : source (option float)
                              , PresDisTime : source (option float)
                              , ChanEnaTime : source (option float)
                              , ExecActTime : source (option float)
                  }} =

    kill_p <- source False;
    timed_g <- source False;
    cn_time <- source (Some 0.);
    gv_time <- source (Some 0.);
    pd_time <- source (Some 0.);
    ce_time <- source (Some 0.);
    ea_time <- source (Some 0.);

    change_rules_id <- fresh;

    let fun submit_new_rules r : transaction {} =
            dml (DELETE FROM rule_set
                        WHERE Room = {[rt.Room]}
                          AND Game = {[rt.CurrentGame]});
            set_rules rt r;
            send_public_message gt (RuleSet r)

        fun change_rules_view {} : xbody = <xml>
          <div class={cl (B.modal :: B.fade :: [])} id={change_rules_id} role="dialog">
            <div class={B.modal_dialog} role="document">
              <div class={B.modal_content}>
                <div class={B.modal_header}>
                  <button class={B.close} data-dismiss="modal" aria-label="Close">
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
                          onclick={fn _ =>
                                      kill_p <- get kill_p;
                                      timed_g <- get timed_g;
                                      cn_time <- get cn_time;
                                      gv_time <- get gv_time;
                                      pd_time <- get pd_time;
                                      ce_time <- get ce_time;
                                      ea_time <- get ea_time;
                                      rpc (submit_new_rules
                                               { KillPlayer = kill_p
                                               , TimedGame  = timed_g
                                               , ChanNomTime = z_if_n cn_time
                                               , GovVoteTime = z_if_n gv_time
                                               , PresDisTime = z_if_n pd_time
                                               , ChanEnaTime = z_if_n ce_time
                                               , ExecActTime = z_if_n ea_time
                                          })
                                  }
                          data-dismiss="modal"
                          value="Change it!"/>
        </div></div></div></div></xml>

        fun rule_view {} : xbody =
            <xml>
              <dyn signal={
                kill_p  <- signal kill_p;
                timed_g <- signal timed_g;
                cn_time <- signal cn_time;
                gv_time <- signal gv_time;
                pd_time <- signal pd_time;
                ce_time <- signal ce_time;
                ea_time <- signal ea_time;
                return <xml>
                  <table>
                    <tr><th>{[(if kill_p
                               then "Player killed"
                               else "Turn skipped") ^ " as punishment."]}</th></tr>
                    {if not timed_g
                     then <xml><tr><th>Game untimed</th></tr></xml>
                     else <xml>
                       <tr><th>Chancellor Nomination</th>
                         <td>{[cn_time]}</td></tr>
                       <tr><th>Governmant Vote</th>
                         <td>{[gv_time]}</td></tr>
                       <tr><th>President Discard</th>
                         <td>{[pd_time]}</td></tr>
                       <tr><th>Chancellor Enaction</th>
                         <td>{[ce_time]}</td></tr>
                       <tr><th>Executive Action</th>
                         <td>{[ea_time]}</td></tr>
                       </xml>}</table></xml>}/></xml>

    in  return { Modal = <xml></xml>
               , Button = <xml><button class={cl (B.btn :: B.btn_primary :: [])}
                                       data-toggle="modal"
                                       data-target={"#" ^ show change_rules_id}>Change Rules
                      </button></xml>
               , Sources = { KillPlayer  = kill_p
                           , TimedGame   = timed_g
                           , ChanNomTime = cn_time
                           , GovVoteTime = gv_time
                           , PresDisTime = pd_time
                           , ChanEnaTime = ce_time
                           , ExecActTime = ea_time
                           }}
    end
