open Bootstrap3

open Protocol
open Utils
open Tables
open Types

fun game_view_and_client_handler (pt : player_table)
                                 (rt :   room_table)
                                 (gt :   game_table)
    : transaction { View    : transaction (signal xbody)
                  , Handler : Protocol.in_game_response -> transaction {}
                  } =

    mods <- queryL1 (SELECT * FROM mod WHERE mod.Room = {[rt.Room]});

    (players_s : source (list {Username : string, InGameId : int})) <- source [];

    (page_s : source xbody) <- source (<xml></xml>);

    (msg_s : source (list {Msg : string, Time : time})) <- source [];

    (rule_s : source (option rule_set)) <- source None;

    (game_s : source private_game_state) <- source { PublicGameState =
                                                     { CurrentStep = ChancellorSelectStep
                                                     , CurrentTurn =
                                                       { President       = 0
                                                       , Chancellor      = 0
                                                       , FascistPolicies = 0
                                                       , LiberalPolicies = 0
                                                       , RejectCount     = 0
                                                       }
                                                     , GameHistory = []
                                                     , Players     = []
                                                     }
                                                   , GameRole          = Watcher
                                                   , KnownAffiliations = []
                                                   , Top3CardsInDraw   = []
                                                   };

    (vote_s : source (option (list {Place : int, Vote : bool}))) <- source None;

    (role_s : source turn_role) <- source Voter;

    change_rules_id <- fresh;

    let val msg_loop =
            let fun go {} =
                now <- now;
                get_set msg_s (List.filter (fn m => addSeconds m.Time 15 < now));
                sleep 3;
                go {}
            in spawn (go {}) end

        fun start_game {} = return {}

        fun change_rules_view {} : xbody = <xml>
          <div class="modal fade" id={change_rules_id} role="dialog">
            <div class="modal-dialog" role="document">
              <div class="modal-content">
                <div class="modal-header">
                  <button class="close"
                          data-dismiss="modal"
                          aria-label="Close">
                    <span aria-hidden="true">&times;</span></button>
                  <h4 class="modal-title">Modal title</h4>
                </div>
                <div class="modal-body">
                  <p>One fine body&hellip;</p>
                </div>
                <div class="modal-footer">
                  <button class="btn btn-default" data-dismiss="modal">Close</button>
                  <button class="btn btn-primary">Save changes</button>
                </div>
              </div>
            </div>
          </div>
        </xml>

        fun change_rules {} : transaction {} =
            alert "change rules"

        fun default_view {} : xbody = <xml>
          <dyn signal={players <- signal players_s;
                       rule_set <- signal rule_s;
                       return <xml>
                         <table>
                           {case rule_set of
                                None => <xml></xml>
                              | Some rule_set => <xml>
                                <tr><th>{[(if rule_set.KillPlayer
                                           then "Player killed"
                                           else "Turn skipped") ^ " as punishment."]}</th></tr>
                                {if not rule_set.TimedGame
                                 then <xml><tr><th>Game untimed</th></tr></xml>
                                 else <xml>
                                   <tr><th>Chancellor Nomination</th>
                                     <td>{[rule_set.ChanNomTime]}</td></tr>
                                   <tr><th>Governmant Vote</th>
                                     <td>{[rule_set.GovVoteTime]}</td></tr>
                                   <tr><th>President Discard</th>
                                     <td>{[rule_set.PresDisTime]}</td></tr>
                                   <tr><th>Chancellor Enaction</th>
                                     <td>{[rule_set.ChanEnaTime]}</td></tr>
                                   <tr><th>Executive Action</th>
                                     <td>{[rule_set.ExecActTime]}</td></tr>
                                 </xml>}
                              </xml>}
                           {if rt.OwnedBy <> pt.Player &&
                               not (List.exists (fn m => pt.Player = m.Player) mods)
                            then <xml></xml>
                            else <xml>
                              <tr><td>
                                <button class="btn btn-primary"
                                        data-toggle="modal"
                                        data-target={"#" ^ show change_rules_id}>Change Rules</button>
                              </td></tr>
                              {if List.length players < 5 || List.length players > 10
                               then <xml></xml>
                               else <xml><tr><td>
                                 <button onclick={fn _ => rpc (start_game {})}>Start Game
                                 </button></td></tr></xml>}</xml>}
                           {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>)
                                       players}
                           </table></xml>}></dyn>
          {change_rules_view {}}</xml>

        fun display_vote_state {} : transaction xbody =
            vote_s <- get vote_s;
            case vote_s of
                None => return <xml>No votes yet.</xml>
              | Some _ => return <xml>Votes here</xml>

        fun app_vote (vn : vote_notif)
                     (vl_o : option (list vote_notif))
            : option (list vote_notif) =
            case vl_o of
                None    => Some (vn :: [])
              | Some vl => Some (if List.exists (fn v => vn.Place = v.Place) vl
                                 then vl |> List.mp (fn v => if v.Place = vn.Place
                                                             then vn
                                                             else v)
                                 else vn :: vl)

        fun app_msg (msg : string) : transaction {} =
            now <- now;
            get_set msg_s (fn msg_s => { Msg = msg, Time = now } :: msg_s)

        fun public_rsp_handler (rsp : public_response) : transaction {} =
            case rsp of
                PlayersOnTable ps => set players_s ps
              | NewPlayer p => get_set players_s (fn ps => p :: ps)
              | PlayerLeaves i => get_set players_s (List.filter (fn p => p.InGameId <> i))
              (*| Chat c =>
                update_source_at_2 [#PublicGameState]
                                   [#ChatHistory]
                                   game_s (fn hist => c :: hist)*)
              | RuleSet r => set rule_s (Some r)
              | PublicGameState pgame_s =>
                set page_s (<xml>pgs</xml>);
                update_source_at [#PublicGameState] game_s (fn _ => pgame_s)
              | NewTurn new_turn =>
                app_msg "New Turn!";
                game <- get game_s;
                let val pgs = game.PublicGameState
                    val previous_turn = pgs.CurrentTurn
                in  set game_s (game -- #PublicGameState
                                 ++ { PublicGameState =
                                      pgs -- #CurrentTurn -- #GameHistory
                                          ++ { CurrentTurn = new_turn
                                             , GameHistory =
                                               previous_turn :: pgs.GameHistory }})
                end
              | TurnRole r => return {}
              | ChancellorChosen c =>
                app_msg "Chancellor chosen";
                update_source_at_3 [#PublicGameState]
                                   [#CurrentTurn]
                                   [#Chancellor] game_s (fn _ => c)
              | VoteNotif n => get_set vote_s (app_vote n)
              | NewGovt _ => return {}
              | PresidentDiscard =>
                app_msg "President Discarded policy, waiting for chancellor."
              | PolicyPassed p =>
                app_msg (show p ^ " policy passed!");
                (case p of
                     Fascist =>
                     update_source_at_3 [#PublicGameState]
                                        [#CurrentTurn]
                                        [#FascistPolicies] game_s (fn n => n + 1)
                   | Liberal =>
                     update_source_at_3 [#PublicGameState]
                                        [#CurrentTurn]
                                        [#LiberalPolicies] game_s (fn n => n + 1))
              | PlayersPunished _ =>
                app_msg "Player punished!"
              | ExecutionPower =>
                app_msg "President must execute player."
              | PlayerExecuted _ => return {}
              | VetoEnacted =>
                app_msg "Chancellor and President enacted veto."
              | GameEndState _ => return {}

        fun private_rsp_handler (rsp : private_response) : transaction {} =
            let fun voter_handler r =
                    case r of
                        VoteCapability cap => return {}

                fun president_handler r =
                    case r of
                        SelectCandidate cs => return {}
                      | DiscardCard (one, two, three) => return {}
                      | LoyaltyInvestig player => return {}
                      | PolicyPeek (one, two, three) => return {}
                      | VetoProposed => return {}

                fun chancellor_handler r =
                    case r of
                        Policies policies => return {}
                      | VetoRejected => return {}

                fun fascist_handler r =
                    case r of
                        Affiliations as => return {}

            in  case rsp of
                         VoterRsp r =>      voter_handler r
                  |  PresidentRsp r =>  president_handler r
                  | ChancellorRsp r => chancellor_handler r
                  |    FascistRsp r =>    fascist_handler r
            end

    in  return { View =
                 set page_s (default_view {});
                 return (signal page_s)
               , Handler = fn msg => case msg of
                                         PublicRsp  rsp =>  public_rsp_handler rsp
                                       | PrivateRsp rsp => private_rsp_handler rsp }
    end
