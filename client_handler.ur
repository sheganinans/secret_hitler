open Protocol
open Utils
open Types

fun client_handler_closure
        (page : source xbody)
    : transaction (Protocol.in_game_response -> transaction {}) =
    (msgs : source (list {Msg : string, Time : time})) <- source [];

    (rs : source (option rule_set)) <- source None;

    (gs : source private_game_state)
    <- source { PublicGameState =
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

    (vs : source (option (list {Place : int, Vote : bool}))) <- source None;

    (trs : source turn_role) <- source Voter;

    let
        fun msg_loop {} : transaction {} =
            now <- now;
            get_set msgs
                    (List.filter (fn m => addSeconds m.Time 15 < now));
            sleep 3;
            msg_loop {}

        fun display_vote_state {} : transaction xbody =
            vs <- get vs;
            case vs of
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
            get_set msgs (fn msgs => { Msg = msg, Time = now } :: msgs)

        fun public_rsp_handler (rsp : public_response) : transaction {} =
            case rsp of
                Chat c =>
                update_source_at_2 [#PublicGameState]
                                   [#ChatHistory]
                                   gs (fn hist => c :: hist)
              | RuleSet r => set rs (Some r)
              | PublicGameState pgs =>
                update_source_at [#PublicGameState] gs (fn _ => pgs)
              | NewTurn new_turn =>
                app_msg "New Turn!";
                game <- get gs;
                let val pgs = game.PublicGameState
                    val previous_turn = pgs.CurrentTurn
                in  set gs (game -- #PublicGameState
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
                                   [#Chancellor] gs (fn _ => c)
              | VoteNotif n => get_set vs (app_vote n)
              | NewGovt _ => return {}
              | PresidentDiscard =>
                app_msg "President Discarded policy, waiting for chancellor."
              | PolicyPassed p =>
                app_msg (show p ^ " policy passed!");
                (case p of
                     Fascist =>
                     update_source_at_3 [#PublicGameState]
                                        [#CurrentTurn]
                                        [#FascistPolicies] gs (fn n => n + 1)
                   | Liberal =>
                     update_source_at_3 [#PublicGameState]
                                        [#CurrentTurn]
                                        [#LiberalPolicies] gs (fn n => n + 1))
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

    in return (fn msg => case msg of
                              PublicRsp rsp =>  public_rsp_handler rsp
                           | PrivateRsp rsp => private_rsp_handler rsp)
    end
