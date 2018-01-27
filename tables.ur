open Protocol
open Types
open Utils

type player_table_t = [ Player = int ] ++ player_name_and_pass_t

type player_table = $player_table_t

sequence player_seq
table player : player_table_t PRIMARY KEY Player
    , CONSTRAINT UniqName UNIQUE Username

type room_id_t = [ Room = int ]

type room_pass_t = room_id_t ++ [ Pass = option string ]

type room_table_t
  = room_pass_t
  ++ [ Nam         = string
     , OwnedBy     = int
     , CurrentGame = int
     , InGame      = bool ]

type room_table = $room_table_t

sequence room_seq
table room :
      room_table_t PRIMARY KEY Room
    , CONSTRAINT OwnedByPlayer FOREIGN KEY OwnedBy REFERENCES player (Player)

type room_player_relation_t
  = [ Room   = int
    , Player = int
    , SetBy  = int
    , Time   = time
    ]

type room_player_relation = $room_player_relation_t

table room_player :
      room_player_relation_t PRIMARY KEY (Room, Player)
    , CONSTRAINT   RoomExists FOREIGN KEY Room   REFERENCES room   (Room)
    , CONSTRAINT PlayerExists FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT  SetByExists FOREIGN KEY SetBy  REFERENCES player (Player)

table mod :
      room_player_relation_t PRIMARY KEY (Room, Player)
    , CONSTRAINT IsRoomPlayerRelation
      FOREIGN KEY (Room, Player)
      REFERENCES room_player (Room, Player)

type kick_table_t = room_player_relation_t ++ [ Till = time ]

type kick_table = $kick_table_t

table kick :
      kick_table_t PRIMARY KEY (Room, Player)
    , CONSTRAINT IsRoomPlayerRelation
      FOREIGN KEY (Room, Player)
      REFERENCES room_player (Room, Player)

type game_id_t = [ Game = int, Room = int ]

type game_table_t
  = game_id_t
  ++ [ CurrentTurn = int
     , GameStarted = option time
     , LastAction  = time
     , GameEnded   = option time ]

type game_table = $game_table_t

table game :
      game_table_t PRIMARY KEY (Room, Game)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)

type rule_set_table_t =
     game_id_t ++ rule_set_t

type rule_set_table = $rule_set_table_t

table rule_set :
      rule_set_table_t PRIMARY KEY (Room, Game)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

type in_game_id_per_game_t = [ InGameId = int ] ++ game_id_t

type in_game_id_per_game = $in_game_id_per_game_t

type player_connection_t
  = in_game_id_per_game_t
  ++ [ Player   = int
     , Client   = client
     , Chan     = channel Protocol.in_game_response
     , Watching = bool
     ]

type player_connection = $player_connection_t

table player_in_game :
      player_connection_t PRIMARY KEY (Room, Game, InGameId)
    , CONSTRAINT HasGlobalId FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasRoom FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)

type table_ordering_table_t = game_id_t ++ [ InGameId = int, Place = int ]

type table_ordering_table = $table_ordering_table_t

table table_ordering :
      table_ordering_table_t PRIMARY KEY (Room, Game, Place)
    , CONSTRAINT UniqueOrderingPerGame UNIQUE (Room, Game, Place, InGameId)
    , CONSTRAINT InGame FOREIGN KEY (Room, Game, InGameId)
                 REFERENCES player_in_game (Room, Game, InGameId)

type place_per_game_t = [ Place = int ] ++ game_id_t

type place_per_game = $place_per_game_t

table liberal :
      place_per_game_t
          CONSTRAINT HasPlaceInGame
          FOREIGN KEY (Room, Game, Place)
          REFERENCES table_ordering (Room, Game, Place)

table fascist :
      place_per_game_t
          CONSTRAINT HasPlaceInGame
          FOREIGN KEY (Room, Game, Place)
          REFERENCES table_ordering (Room, Game, Place)

table hitler :
      place_per_game_t
      CONSTRAINT UniqueHilterPerGame UNIQUE (Room, Game, Place)
    , CONSTRAINT HasPlaceInGame
      FOREIGN KEY (Room, Game, Place)
      REFERENCES table_ordering (Room, Game, Place)

type turn_id_t = game_id_t ++ [ Turn = int ]

type current_step_t
  = [ HitlerCheckDone = bool
    , CurrentStep     = serialized step
    ]

type deck_state_t
  = current_decks_t
  ++ [ Fst = serialized side
     , Snd = serialized side
     , Trd = serialized side
     ]

type game_state_t
  = [ PresDisc     = option (serialized card)
    , ChanEnac     = option (serialized card)
    , NextPres     = int
    , VetoProposed = bool
    ]

type turn_table_t = turn_id_t ++ current_step_t ++ govt_state_t ++ deck_state_t ++ game_state_t

type turn_table = $turn_table_t

table turn :
      turn_table_t PRIMARY KEY (Room, Game, Turn)
    , CONSTRAINT  PresidentInOrdering FOREIGN KEY (Room, Game, President)
                 REFERENCES table_ordering (Room, Game, Place)
    , CONSTRAINT   NextPresInOrdering FOREIGN KEY (Room, Game, NextPres)
                 REFERENCES table_ordering (Room, Game, Place)
    , CONSTRAINT ChancellorInOrdering FOREIGN KEY (Room, Game, Chancellor)
                 REFERENCES table_ordering (Room, Game, Place)
    , CONSTRAINT UniquePresident  UNIQUE (Room, Game, Turn, President)
    , CONSTRAINT UniqueNextPres   UNIQUE (Room, Game, Turn, NextPres)
    , CONSTRAINT UniqueChancellor UNIQUE (Room, Game, Turn, Chancellor)
    , CONSTRAINT     PresNotDummy CHECK President <> 0
    , CONSTRAINT NextPresNotDummy CHECK  NextPres <> 0
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)
    , CONSTRAINT PresIsPlayer FOREIGN KEY (Room, Game, President)
                 REFERENCES table_ordering (Room, Game, Place)
    , CONSTRAINT NextPresIsPlayer FOREIGN KEY (Room, Game, NextPres)
                 REFERENCES table_ordering (Room, Game, Place)
    , CONSTRAINT ChancellorIsPlayer FOREIGN KEY (Room, Game, Chancellor)
                 REFERENCES table_ordering (Room, Game, Place)
    , CONSTRAINT RejectCount CHECK RejectCount >= 0 AND RejectCount <= 3
    , CONSTRAINT AllowedEnac CHECK IF ChanEnac <> NULL THEN PresDisc <> NULL ELSE TRUE
(*    , CONSTRAINT ChancSeldNotDummy CHECK IF CurrentStep = {[serialize VoteStep]}
                                         THEN Chancellor <> 0 ELSE TRUE
    , CONSTRAINT DiscardNotDummy   CHECK IF CurrentStep = {[serialize EnactStep]}
                                         THEN PresDisc <> {[None]} ELSE TRUE
    , CONSTRAINT EnactionNotDummy CHECK IF CurrentStep = {[serialize ExecActionStep]}
                                         THEN ChanEnac <> NULL ELSE TRUE
*)    , CONSTRAINT DrawDeckGTE0 CHECK LiberalsInDraw + FascistsInDraw >= 0
    , CONSTRAINT LibPolGTE0AndLTE5 CHECK LiberalPolicies >= 0 AND LiberalPolicies <= 5
    , CONSTRAINT FasPolGTE0AndLTE5 CHECK FascistPolicies >= 0 AND FascistPolicies <= 6
    , CONSTRAINT LiberalsUnchanging
          CHECK LiberalsInDraw
              + LiberalsInDisc
              + LiberalPolicies
              + (IF Fst = {[serialize Liberal]} THEN 1 ELSE 0)
              + (IF Snd = {[serialize Liberal]} THEN 1 ELSE 0)
              + (IF Trd = {[serialize Liberal]} THEN 1 ELSE 0)
              = {[number_of_liberal_policies]}
    , CONSTRAINT FascistsUnchanging
          CHECK FascistsInDraw
              + FascistsInDisc
              + FascistPolicies
              + (IF Fst = {[serialize Fascist]} THEN 1 ELSE 0)
              + (IF Snd = {[serialize Fascist]} THEN 1 ELSE 0)
              + (IF Trd = {[serialize Fascist]} THEN 1 ELSE 0)
              = {[number_of_fascist_policies]}

type capability_table_t
  = turn_id_t
  ++ [ Place      = int
     , Capability = int
     , Action     = serialized Types.action
     ]

type capability_table = $capability_table_t

table capability :
      capability_table_t PRIMARY KEY (Room, Game, Turn, Place, Capability)
    , CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
    , CONSTRAINT InGameOrdering FOREIGN KEY (Room, Game, Place)
                   REFERENCES table_ordering (Room, Game, Place)

type vote_table_t = turn_id_t ++ [ Place = int, Vote = option bool ]

type vote_table = $vote_table_t

table vote :
      vote_table_t PRIMARY KEY (Room, Game, Turn, Place)
      , CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT InGameOrdering FOREIGN KEY (Room, Game, Place)
                   REFERENCES table_ordering (Room, Game, Place)

type president_action_table_t = turn_id_t ++ [ Place = int ]

type president_action_table = $president_action_table_t

table loyalty_investigation :
      president_action_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT InGameOrdering FOREIGN KEY (Room, Game, Place)
                   REFERENCES table_ordering (Room, Game, Place)
      , CONSTRAINT UniqueInvestigation UNIQUE (Room, Game, Turn)

table dead_player :
      president_action_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT InGameOrdering FOREIGN KEY (Room, Game, Place)
                   REFERENCES table_ordering (Room, Game, Place)
      , CONSTRAINT UniqueDeathPerGame UNIQUE (Room, Game, Place)

type veto_table_t = turn_id_t ++ [ President = int, Chancellor = int ]

table veto :
      veto_table_t
      CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)


type chat_id_t = [ Chat = int ]

sequence chat_seq
table chat : chat_id_t PRIMARY KEY Chat

type chat_table_t = chat_id_t ++ [ Player = int, Time = time, Text = string ]

type chat_table = $chat_table_t

type direct_chat_table_t = chat_table_t ++ [ Recipient = int ]

table direct_chat :
      direct_chat_table_t PRIMARY KEY Chat
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT RecipientIsPlayer FOREIGN KEY Recipient REFERENCES player (Player)

type room_chat_table_t = chat_table_t ++ [ Room = int ]

type room_chat_table = $room_chat_table_t

table room_chat :
      room_chat_table_t PRIMARY KEY Chat
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)
    , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT InRoom FOREIGN KEY (Room, Player) REFERENCES room_player (Room, Player)

table mod_chat :
      room_chat_table_t PRIMARY KEY Chat
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)
    , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT IsMod FOREIGN KEY (Room, Player) REFERENCES mod (Room, Player)

type kicked_chat_table_t = room_chat_table_t ++ [ Kicked = int ]

type kicked_chat_table = $kicked_chat_table_t

table kicked_chat :
      kicked_chat_table_t PRIMARY KEY Chat
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT AboutKicked FOREIGN KEY (Room, Kicked) REFERENCES kick (Room, Player)

type game_chat_table_t = room_chat_table_t ++ [ Game = int ]

table game_chat :
      game_chat_table_t PRIMARY KEY Chat
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)

type chat_player_relation_t = [ Chat = int, Player = int ]

table group_chat_relation :
      chat_player_relation_t PRIMARY KEY (Chat, Player)
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

table group_chat :
      chat_table_t PRIMARY KEY Chat
    , CONSTRAINT HasChat FOREIGN KEY Chat REFERENCES chat (Chat)
    , CONSTRAINT InGroup FOREIGN KEY (Chat, Player) REFERENCES group_chat_relation (Chat, Player)
    , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)


task periodic 60 = (* Kick removal loop *)
     fn {} =>
        now <- now;
        dml (DELETE FROM kick WHERE Till < {[now]})

fun get_rule_set [rest] [game_id_t ~ rest]
                 (t : $(game_id_t ++ rest)) : transaction rule_set_table =
    oneRow1 (SELECT *
             FROM rule_set
             WHERE rule_set.Room = {[t.Room]}
               AND rule_set.Game = {[t.Game]})

fun get_player_from_in_game_id (rt : room_table) (pid : int) : transaction { Player : int } =
    oneRow1 (SELECT player_in_game.Player
             FROM player_in_game
             WHERE player_in_game.Room = {[rt.Room]}
               AND player_in_game.Game = {[rt.CurrentGame]}
               AND player_in_game.InGameId = {[pid]})

fun update_last_action (gt : game_table) =
    now <- now;
    dml (UPDATE game
         SET LastAction = {[now]}
         WHERE Room = {[gt.Room]}
           AND Game = {[gt.Game]})

fun no_longer_in_game [rest] [room_id_t ~ rest]
                      (t : $(room_id_t ++ rest)) : transaction {} =
    dml (UPDATE room
         SET InGame = FALSE
         WHERE Room = {[t.Room]})

fun active_games {} : transaction (list game_table) =
    queryL1 (SELECT game.*
             FROM (game
                 INNER JOIN room
                 ON  game.Room = room.Room
                 AND game.Game = room.CurrentGame))

fun current_turn_state (gt : game_table) : transaction turn_table =
    oneRow1 (SELECT *
             FROM turn
             WHERE turn.Room = {[gt.Room]}
               AND turn.Game = {[gt.Game]}
               AND turn.Turn = {[gt.CurrentTurn]})

fun players_in_game (gt : game_table) : transaction (list player_connection) =
    queryL1 (SELECT *
             FROM player_in_game
             WHERE player_in_game.Room = {[gt.Room]}
               AND player_in_game.Game = {[gt.Game]})

fun number_of_players_in_room_for_game (rt : room_table) : transaction int =
    oneRowE1 (SELECT COUNT ( * )
              FROM player_in_game
              WHERE player_in_game.Room = {[rt.Room]}
                AND player_in_game.Game = {[rt.CurrentGame]})

fun get_hitler [rest] [game_id_t ~ rest]
               (t : $(game_id_t ++ rest)) : transaction place_per_game =
    oneRow1 (SELECT *
             FROM hitler
             WHERE hitler.Room = {[t.Room]}
               AND hitler.Game = {[t.Game]})

fun get_fascists [rest] [game_id_t ~ rest]
                 (t : $(game_id_t ++ rest)) : transaction (list place_per_game) =
    queryL1 (SELECT *
             FROM fascist
             WHERE fascist.Room = {[t.Room]}
               AND fascist.Game = {[t.Game]})

fun get_liberals [rest] [game_id_t ~ rest]
                 (t : $(game_id_t ++ rest)) : transaction (list place_per_game) =
    queryL1 (SELECT *
             FROM liberal
             WHERE liberal.Room = {[t.Room]}
               AND liberal.Game = {[t.Game]})

fun get_dead [rest] [game_id_t ~ rest]
             (t : $(game_id_t ++ rest)) : transaction (list president_action_table) =
    queryL1 (SELECT *
             FROM dead_player
             WHERE dead_player.Room = {[t.Room]}
               AND dead_player.Game = {[t.Game]})

fun add_player_to_game (rt : room_table)
                       (pt : player_table)
                       (me : client)
                       (chan : channel Protocol.in_game_response)
                       (playing : bool)
                       (num_in_game : int) : transaction {} =
    dml (INSERT INTO player_in_game
           ( Room
             , Game
             , Player
             , Client
             , Chan
             , Watching
             , InGameId )
         VALUES
           ( {[rt.Room]}
             , {[rt.CurrentGame]}
             , {[pt.Player]}
             , {[me]}
             , {[chan]}
             , {[if rt.InGame then True else not playing]}
             , {[num_in_game + 1]} ))

fun player_at_table [rest] [game_id_t ~ rest]
                    (t : $(game_id_t ++ rest))
                    (place : int) : transaction (table_ordering_table) =
    oneRow1 (SELECT *
             FROM table_ordering
             WHERE table_ordering.Room = {[t.Room]}
               AND table_ordering.Game = {[t.Game]}
               AND table_ordering.Place = {[place]})

fun player_connection_in_game [rest] [game_id_t ~ rest]
                              (t : $(game_id_t ++ rest))
                              (in_game_id : int) : transaction player_connection =
    oneRow1 (SELECT *
             FROM player_in_game
             WHERE player_in_game.Room = {[t.Room]}
               AND player_in_game.Game = {[t.Game]}
               AND player_in_game.InGameId = {[in_game_id]})

fun send_public_message (gt : game_table) (msg : public_response) : transaction {} =
    player_list <- players_in_game gt;
    mapM_ (fn p => send p.Chan (PublicRsp msg)) player_list

fun get_capability (tt : turn_table)
                   (place : int)
                   (cap_id : int) : transaction (option capability_table) =
    oneOrNoRows1 (SELECT *
                  FROM capability
                  WHERE capability.Room = {[tt.Room]}
                    AND capability.Game = {[tt.Game]}
                    AND capability.Turn = {[tt.Turn]}
                    AND capability.Place = {[place]}
                    AND capability.Capability = {[cap_id]})

fun get_players_playing (gt : game_table) : transaction (list { Username : string
                                                              , InGameId : int
                                                          }) =
    queryL (SELECT player.Username AS Username, PIG.InGameId AS InGameId
             FROM (SELECT player_in_game.Player   AS Player
                        , player_in_game.InGameId AS InGameId
                   FROM player_in_game
                   WHERE player_in_game.Room = {[gt.Room]}
                     AND player_in_game.Game = {[gt.Game]}
                     AND player_in_game.Watching = FALSE) AS PIG
             JOIN player ON PIG.Player = player.Player)

fun alive_player_ordering (gt : game_table) : transaction (list table_ordering_table) =
    queryL1 (SELECT table_ordering.*
             FROM (table_ordering
                 LEFT JOIN dead_player
                 ON  table_ordering.Room  = dead_player.Room
                 AND table_ordering.Game  = dead_player.Game
                 AND table_ordering.Place = dead_player.Place)
             WHERE table_ordering.Room = {[gt.Room]}
               AND table_ordering.Game = {[gt.Game]})

fun previous_turn (tt : turn_table) : transaction (option turn_table) =
    oneOrNoRows1 (SELECT *
                  FROM turn
                  WHERE turn.Room = {[tt.Room]}
                    AND turn.Game = {[tt.Game]}
                    AND turn.Turn = {[tt.Turn - 1]})

fun president_veto_turn (tt : turn_table) : transaction {} =
    dml (INSERT INTO veto (Room, Game, Turn, President, Chancellor)
         VALUES ({[tt.Room]}
             , {[tt.Game]}
             , {[tt.Turn]}
             , {[tt.President]}
             , {[tt.Chancellor]}))

(* TODO update option to result to model state of place not being in turn. *)
fun possible_liberal (rt : room_table) (place : int) : transaction (option {Place : int}) =
    oneOrNoRows1 (SELECT liberal.Place
                  FROM liberal
                  WHERE liberal.Room = {[rt.Room]}
                    AND liberal.Game = {[rt.CurrentGame]}
                    AND liberal.Place = {[place]})

fun possible_hitler (tt : turn_table) : transaction (option {Place : int}) =
    oneOrNoRows1 (SELECT hitler.Place
                  FROM hitler
                  WHERE hitler.Room = {[tt.Room]}
                    AND hitler.Game = {[tt.Game]}
                    AND hitler.Place = {[tt.Chancellor]})

fun set_rules (rt : room_table) (rs : rule_set) : transaction {} =
    if rt.InGame
    then return {}
    else
        dml (INSERT INTO rule_set
               ( Room
                 , Game
                 , TimedGame
                 , KillPlayer
                 , ChanNomTime
                 , GovVoteTime
                 , PresDisTime
                 , ChanEnaTime
                 , ExecActTime )
             VALUES
               ({[rt.Room]}
                 , {[rt.CurrentGame]}
                 , {[rs.TimedGame]}
                 , {[rs.KillPlayer]}
                 , {[rs.ChanNomTime]}
                 , {[rs.GovVoteTime]}
                 , {[rs.PresDisTime]}
                 , {[rs.ChanEnaTime]}
                 , {[rs.ExecActTime]} ))

fun submit_chancellor (tt : turn_table) (chan_id : int) : transaction {} =
    dml (UPDATE turn
         SET Chancellor = {[chan_id]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]});
    dml (UPDATE turn
         SET CurrentStep = {[serialize VoteStep]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]})

fun submit_discard (tt : turn_table) (c : card) : transaction {} =
    dml (UPDATE turn
         SET PresDisc = {[Some (serialize c)]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]});
    dml (UPDATE turn
         SET CurrentStep = {[serialize EnactStep]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]})

fun submit_loyalty_investigation (tt : turn_table) (place : int) : transaction {} =
    dml (INSERT INTO loyalty_investigation (Room, Game, Turn, Place)
         VALUES ({[tt.Room]}, {[tt.Game]}, {[tt.Turn]}, {[place]}))

fun kill_player (tt : turn_table) (place : int) : transaction {} =
    dml (INSERT INTO dead_player (Room, Game, Turn, Place)
         VALUES ({[tt.Room]}, {[tt.Game]}, {[tt.Turn]}, {[place]}))

fun does_vote_exist_for (tt : turn_table) (place : int) : transaction (option vote_table) =
    oneOrNoRows1 (SELECT *
                  FROM vote
                  WHERE vote.Room  = {[tt.Room]}
                    AND vote.Game  = {[tt.Game]}
                    AND vote.Turn  = {[tt.Turn]}
                    AND vote.Place = {[place]})

fun new_vote (tt : turn_table) (place : int) (a : option bool) : transaction {} =
    dml (INSERT INTO vote (Room, Game, Turn, Place, Vote)
         VALUES ( {[tt.Room]}
             , {[tt.Game]}
             , {[tt.Turn]}
             , {[place]}
             , {[a]} ))

fun update_vote (tt : turn_table) (place : int) (a : option bool) : transaction {} =
    dml (UPDATE vote
         SET Vote = {[a]}
         WHERE Room  = {[tt.Room]}
           AND Game  = {[tt.Game]}
           AND Turn  = {[tt.Turn]}
           AND Place = {[place]})

fun current_votes (tt : turn_table) : transaction (list vote_table) =
    queryL1 (SELECT *
             FROM vote
             WHERE vote.Room = {[tt.Room]}
               AND vote.Game = {[tt.Game]}
               AND vote.Turn = {[tt.Turn]})

fun incr_reject_counter (tt : turn_table) : transaction {} =
    dml (UPDATE turn
         SET RejectCount = {[tt.RejectCount + 1]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]})

fun reset_reject_counter (tt : turn_table) : transaction {} =
    dml (UPDATE turn
         SET RejectCount = 0
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]})

fun vote_done (tt : turn_table) : transaction {} =
    dml (UPDATE turn
         SET CurrentStep = {[serialize DiscardStep]}
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]})

fun hitler_check_done (tt : turn_table) : transaction {} =
    dml (UPDATE turn
         SET HitlerCheckDone = TRUE
         WHERE Room = {[tt.Room]}
           AND Game = {[tt.Game]}
           AND Turn = {[tt.Turn]})

fun incr_curr_turn (rt : room_table) : transaction {} =
    dml (UPDATE game
         SET CurrentTurn = 1
         WHERE Room = {[rt.Room]}
           AND Game = {[rt.CurrentGame]})

fun skip_turn (gt : game_table) : transaction {} =
    current_turn <- current_turn_state gt;
    deck <- next_turn_deck_state current_turn;

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
           ( {[current_turn.Room]}
             , {[current_turn.Game]}
             , {[current_turn.Turn + 1]}
             , {[current_turn.NextPres]}
             , {[current_turn.NextPres + 1]} (* TODO add rollover and skipping dead *)
             , 0
             , {[current_turn.RejectCount + 1]}
             , FALSE
             , FALSE
             , {[serialize ChancellorSelectStep (*TODO incr*)]}
             , {[deck.LibDraw]}
             , {[deck.FasDraw]}
             , {[deck.LibDisc]}
             , {[deck.FasDisc]}
             , {[serialize deck.Fst]}
             , {[serialize deck.Snd]}
             , {[serialize deck.Trd]}
             , NULL
             , NULL
             , {[current_turn.LiberalPolicies]}
             , {[current_turn.FascistPolicies]} ))
