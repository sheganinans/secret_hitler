open Protocol
open Utils
open Tables
open Types

fun default_game_view {} : transaction xbody =
    return <xml>gv</xml>

fun client_view_closure {}
    : transaction (source xbody * (Protocol.in_game_response -> transaction {})) =

    default_view <- default_game_view {};

    (page : source xbody) <- source default_view;

    (msg_s : source (list {Msg : string, Time : time})) <- source [];

    (rule_s : source (option rule_set)) <- source None;

    (game_s : source private_game_state) <- source { PublicGameState =
                                                     { CurrentTurn =
                                                       { President       = 0
                                                       , Chancellor      = 0
                                                       , FascistPolicies = 0
                                                       , LiberalPolicies = 0
                                                       , RejectCount     = 0
                                                       }
                                                     , GameHistory = []
                                                     , ChatHistory = []
                                                     , Players     = []
                                                     }
                                                   , GameRole          = Watcher
                                                   , KnownAffiliations = []
                                                   , Top3CardsInDraw   = []
                                                   };

    (vote_s : source (option (list {Place : int, Vote : bool}))) <- source None;

    (role_s : source turn_role) <- source Voter;

    let val msg_loop =
            let fun go {} =
                now <- now;
                get_set msg_s (List.filter (fn m => addSeconds m.Time 15 < now));
                sleep 3;
                go {}
            in spawn (go {}) end

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

        fun app_msg msg =
            now <- now;
            get_set msg_s (fn msg_s => { Msg = msg, Time = now } :: msg_s)

        fun public_rsp_handler (rsp : public_response) : transaction {} =
            case rsp of
                PlayersOnTable _ => return {}
              | Chat c =>
                update_source_at_2 [#PublicGameState]
                                   [#ChatHistory]
                                   game_s (fn hist => c :: hist)
              | RuleSet r => set rule_s (Some r)
              | PublicGameState pgame_s =>
                update_source_at [#PublicGameState] game_s (fn _ => pgame_s)
              | NewTurn new_turn =>
                app_msg "New Turn!";
                game <- get game_s;
                let val pgame_s = game.PublicGameState
                    val previous_turn = pgame_s.CurrentTurn
                in  set game_s (game -- #PublicGameState
                                 ++ { PublicGameState =
                                      pgame_s -- #CurrentTurn -- #GameHistory
                                          ++ { CurrentTurn = new_turn
                                             , GameHistory =
                                               previous_turn :: pgame_s.GameHistory }})
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

    in  return (page, fn msg => case msg of
                                    PublicRsp rsp =>  public_rsp_handler rsp
                                  | PrivateRsp rsp => private_rsp_handler rsp)
    end
