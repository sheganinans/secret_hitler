open Frontend

fun eval_capability (arg : option capability_arg)
                    (room_id : int)
                    (cap_id  : int) : transaction {} =
    pigot <- player_in_game_on_turn_exn room_id;
    let val (rt, gt, tt, ot) = (pigot.Room, pigot.Game, pigot.Turn, pigot.TableOrder)

        val send_public_message = send_public_message gt

        fun set_rule_set {} : transaction {} =
            case arg of
                Some (RuleSetArg rs) =>
                set_rules rt rs;
                send_public_message (RuleSet rs)
              | _ => return {}

        fun start_game {} : transaction {} =
            player_list <- queryL1 (SELECT *
                                    FROM player_in_game
                                    WHERE player_in_game.Room = {[rt.Room]}
                                      AND player_in_game.Game = {[rt.CurrentGame]}
                                    ORDER BY RANDOM ());
            let val in_game_players = List.filter (fn p => not p.Watching) player_list
                val num_in_game_players = List.length in_game_players
                val get_pid = get_player_from_in_game_id rt
                val pres_pid = 1
                val chanc_pid = 2
                val lib_pol = 0
                val fas_pol = 0

            in  case List.find (fn (k,_) => k = num_in_game_players) player_numbers_table of
                    None => return {}
                  | Some (_,lf) =>
                    in_game_players <- Utils.shuffle in_game_players;

                    mapM_ (fn (i,p) =>
                              dml (INSERT INTO table_ordering (Room, Game, InGameId, Place)
                                   VALUES
                                     ( {[rt.Room]}
                                       , {[rt.CurrentGame]}
                                       , {[p.InGameId]}
                                       , {[i]} )))
                          (List.mapi (fn i p => (i+1,p)) in_game_players);

                    in_ordering_players
                        <- queryL1 (SELECT *
                                    FROM table_ordering
                                    WHERE table_ordering.Room = {[rt.Room]}
                                      AND table_ordering.Game = {[rt.CurrentGame]});

                    in_ordering_players <- Utils.shuffle in_ordering_players;

                    let val players : { Lib : list table_ordering_table
                                      , Hit :      table_ordering_table
                                      , Fas : list table_ordering_table } =
                            { Lib = List.take lf.Liberals in_ordering_players
                            , Hit = Option.unsafeGet (List.nth in_ordering_players lf.Liberals)
                            , Fas = List.drop (lf.Liberals + 1) in_ordering_players }

                    in  mapM_ (fn i => dml (INSERT INTO liberal (Room, Game, Place)
                                            VALUES
                                              ( {[rt.Room]}
                                                , {[rt.CurrentGame]}
                                                , {[i.Place]} )))
                              players.Lib;

                        dml (INSERT INTO hitler (Room, Game, Place)
                             VALUES
                               ( {[rt.Room]}
                                 , {[rt.CurrentGame]}
                                 , {[players.Hit.Place]} ));

                        mapM_ (fn i => dml (INSERT INTO fascist (Room, Game, Place)
                                            VALUES
                                              ( {[rt.Room]}
                                                , {[rt.CurrentGame]}
                                                , {[i.Place]} )))
                              players.Fas;

                        deck <- next_turn_deck_state
                                    { LiberalsInDraw = number_of_liberal_policies
                                    , FascistsInDraw = number_of_fascist_policies
                                    , LiberalsInDisc = 0
                                    , FascistsInDisc = 0 };

                        dml (INSERT INTO turn
                               ( Room
                                 , Game
                                 , Turn
                                 , President
                                 , NextPres
                                 , Chancellor
                                 , RejectCount
                                 , VetoProposed
                                 , HitlerCheckDone
                                 , CurrentStep
                                 , LiberalsInDraw
                                 , FascistsInDraw
                                 , LiberalsInDisc
                                 , FascistsInDisc
                                 , Fst
                                 , Snd
                                 , Trd
                                 , PresDisc
                                 , ChanEnac
                                 , LiberalPolicies
                                 , FascistPolicies )
                             VALUES
                               ( {[rt.Room]}
                                 , {[rt.CurrentGame]}
                                 , 1
                                 , {[pres_pid]}
                                 , {[chanc_pid]}
                                 , 0
                                 , 0
                                 , FALSE
                                 , FALSE
                                 , {[serialize ChancellorSelectStep]}
                                 , {[deck.LibDraw]}
                                 , {[deck.FasDraw]}
                                 , 0
                                 , 0
                                 , {[serialize deck.Fst]}
                                 , {[serialize deck.Snd]}
                                 , {[serialize deck.Trd]}
                                 , NULL
                                 , NULL
                                 , {[lib_pol]}
                                 , {[fas_pol]} ));

                        dml (UPDATE room
                             SET InGame = TRUE
                             WHERE Room = {[rt.Room]});

                        update_last_action gt;
                        incr_curr_turn rt;

                        send_public_message (NewTurn { President       =  pres_pid
                                                     , Chancellor      = chanc_pid
                                                     , LiberalPolicies = lib_pol
                                                     , FascistPolicies = fas_pol
                                                     , RejectCount     = 0 })
                    end
            end

        fun choose_chancellor (chan_id : int) : transaction {} =
            submit_chancellor tt chan_id;
            send_public_message (ChancellorChosen chan_id)

        fun discard_policy (c : card) : transaction {} =
            submit_discard tt c;
            send_public_message PresidentDiscard

        fun enact_policy (c : card) : transaction {} =
            let val chosen_policy : side = deserialize (case c of Fst => tt.Fst
                                                                | Snd => tt.Snd
                                                                | Trd => tt.Trd)
            in  send_public_message (PolicyPassed chosen_policy) end

        fun eval_vote (v : option bool) : transaction {} =
            vote_o <- does_vote_exist_for tt ot.Place;
            (case vote_o of None   =>    new_vote
                          | Some _ => update_vote) tt ot.Place v;
            send_public_message (VoteNotif { Place = ot.Place
                                           , Vote = Option.isSome vote_o })

        fun eval_exec_action (a : Types.exec_action) : transaction {} =
            let fun investigate_loyalty (place : int) : transaction {} =
                    submit_loyalty_investigation tt place;
                    pres_ord <- player_at_table tt tt.President;
                    president <- player_connection_in_game tt pres_ord.InGameId;
                    liberal_o <- possible_liberal rt place;
                    send president.Chan
                         (PrivateRsp (PresidentRsp (LoyaltyInvestig
                             { Place = place
                             , Side  = case liberal_o of
                                           None   => Fascist
                                         | Some _ => Liberal })))

                fun call_special_election (place : int) : transaction {} =
                    return {}

                fun execute_player (place : int) : transaction {} =
                    kill_player tt place;
                    send_public_message (PlayerExecuted place)

                fun propose_veto {} : transaction {} =
                    pres_ord <- player_at_table tt tt.President;
                    president_in_game <- player_connection_in_game tt pres_ord.InGameId;
                    send president_in_game.Chan (PrivateRsp (PresidentRsp VetoProposed))

                fun reject_veto {} : transaction {} =
                    chanc_ord <- player_at_table tt tt.Chancellor;
                    chancellor_in_game <- player_connection_in_game tt chanc_ord.InGameId;
                    send chancellor_in_game.Chan (PrivateRsp (ChancellorRsp VetoRejected))

                fun president_veto {} : transaction {} =
                    president_veto_turn tt;
                    send_public_message VetoEnacted

            in  case a of InvestigateLoyalty  place => investigate_loyalty   place
                        | CallSpecialElection place => call_special_election place
                        | ExecutePlayer       place => execute_player        place
                        | ProposeVeto               =>   propose_veto {}
                        |  RejectVeto               =>    reject_veto {}
                        |        Veto               => president_veto {}
            end

        fun run_timed (a : timed_action) : transaction {} =
            update_last_action gt;
            case a of ChooseChancellor c => choose_chancellor c
                    |    DiscardPolicy p =>    discard_policy p
                    |      EnactPolicy p =>      enact_policy p
                    | ExecutiveAction ea => eval_exec_action ea

    in  capability_o <- get_capability tt ot.Place cap_id;
        case capability_o of
            None => return {}
          | Some cap => case deserialize cap.Action of
                            RoomAction a => (case a of
                                                 SetRuleSet => set_rule_set {}
                                               | StartGame  =>   start_game {})
                          | GameAction a => (case a of
                                                 Vote  v => eval_vote v
                                               | Timed a => run_timed a)
    end
