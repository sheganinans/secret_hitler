structure B = Bootstrap3

open Protocol
open Utils
open Tables
open Types

datatype cilent_state
  = NewGame
  | InGame

structure A = Auth.AuthSystem(struct
                                  fun check_role r =
                                      Auth.check_role_closure (fn _ => <xml></xml>) r
                              end)

fun get_mods (i : int) : transaction (list room_player_relation) =
    queryL1 (SELECT * FROM mod WHERE mod.Room = {[i]})

fun get_curr_game (rt : room_table) =
    oneRow1 (SELECT *
             FROM game
             WHERE game.Room = {[rt.Room]}
               AND game.Game = {[rt.CurrentGame]})

fun get_mods (rt : room_table) : transaction (list room_player_relation) =
    queryL1 (SELECT * FROM mod WHERE mod.Room = {[rt.Room]})


fun handler (pt : player_table)
            (rt :   room_table)
            (gt :   game_table)
            (page_s : source xbody)
            (m : Protocol.in_game_response) : transaction {} =

    rule_change_w <- Rule_changing_widget.rule_changing_widget rt gt;

    mods <- rpc (get_mods rt);

    players_s <- source [];

    msg_s <- source [];

    start_game_cap_s <- source None;

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

    vote_s <- source None;

    role_s <- source Voter;

    let val msg_loop =
            let fun go {} =
                now <- now;
                get_set msg_s (List.filter (fn m => addSeconds m.Time 15 < now));
                sleep 3;
                go {}
            in spawn (go {}) end

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

        fun display_vote_state {} : transaction xbody =
            vote_s <- get vote_s;
            case vote_s of
                None => return <xml>No votes yet.</xml>
              | Some _ => return <xml>Votes here</xml>


            fun start_game {} : transaction {} =
                room_t <- oneRow1 (SELECT *
                                   FROM room
                                   WHERE room.Room = {[gt.Room]});
                if room_t.InGame
                then return {}
                else
                    players <- queryL1 (SELECT *
                                        FROM player_in_game
                                        WHERE player_in_game.Room = {[gt.Room]}
                                          AND player_in_game.Game = {[gt.Game]}
                                          AND player_in_game.Watching = FALSE);
                    if List.length players < 0 || List.length players > 10 (* TODO: 0 *)
                    then return {}
                    else
                        dml (UPDATE room
                             SET InGame = TRUE
                             WHERE Room = {[gt.Room]});

                        starting_players <- get_starting_players gt;

                        update_last_action gt;
                        send_public_message gt (PublicGameState
                                                    { CurrentStep = ChancellorSelectStep
                                                    , CurrentTurn =
                                                      { President       = 1
                                                      , Chancellor      = 0
                                                      , FascistPolicies = 0 (* TODO Alt rules *)
                                                      , LiberalPolicies = 0 (* TODO Alt rules *)
                                                      , RejectCount     = 0
                                                      }
                                                    , GameHistory = []
                                                    , Players     = starting_players
                                               })



        fun default_view {} : transaction xbody =
            return <xml><dyn signal={
            players <- signal players_s;
            return <xml>
              <table>
                {if rt.OwnedBy <> pt.Player &&
                    not (List.exists (fn m => pt.Player = m.Player) mods)
                 then <xml></xml>
                 else <xml>
                   <tr><td>
                   {rule_change_w.Button}
                   </td></tr>
                   {if List.length players < 0 || List.length players > 10 (* TODO: 0 *)
                    then <xml></xml>
                    else <xml><tr><td>
                      <button class={cl (B.btn :: B.btn_default :: [])}
                                        onclick={fn _ => rpc (start_game {})}>Start Game
                      </button></td></tr></xml>}</xml>}
                  {List.mapX (fn p => <xml><tr><td>{[p.Username]}</td></tr></xml>) players}
                </table></xml>}></dyn>
              {rule_change_w.Modal}</xml>

        fun public_rsp_handler (rsp : public_response) : transaction {} =
            case rsp of
                PlayersOnTable ps => set players_s ps
              | NewPlayer p => get_set players_s (fn ps => p :: ps)
              | PlayerLeaves i => get_set players_s (List.filter (fn p => p.InGameId <> i))
              | RuleSet rules =>
                let val s = rule_change_w.Sources
                in  set s.KillPlayer rules.KillPlayer;
                    set s.TimedGame rules.TimedGame;
                    set s.ChanNomTime (Some rules.ChanNomTime);
                    set s.GovVoteTime (Some rules.GovVoteTime);
                    set s.PresDisTime (Some rules.PresDisTime);
                    set s.ChanEnaTime (Some rules.ChanEnaTime);
                    set s.ExecActTime (Some rules.ExecActTime)
                end
              | PublicGameState pgs =>
                set page_s (<xml>{List.mapX (fn p =>
                                                <xml>Un:{[p.Username]}
                                                  <br/>Place:{[p.Place]}
                                                  <br/><br/></xml>)
                                            pgs.Players}</xml>);
                update_source_at [#PublicGameState] game_s (fn _ => pgs)
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

                fun mod_handler r =
                    case r of
                        GameCap r =>
                        set start_game_cap_s
                            (case r of
                                 StartGameCap c => Some c
                               |   RemGameCap   => None)

            in  case rsp of
                    VoterRsp r =>      voter_handler r
                  |  PresidentRsp r =>  president_handler r
                  | ChancellorRsp r => chancellor_handler r
                  |    FascistRsp r =>    fascist_handler r
                  |        ModRsp r =>        mod_handler r
            end
    in  case m of
            PublicRsp  rsp =>  public_rsp_handler rsp
          | PrivateRsp rsp => private_rsp_handler rsp
    end

(*

*)
