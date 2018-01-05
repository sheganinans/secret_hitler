open Types

type player_table_t = [ Player = int ] ++ player_name_and_pass_t

type player_table = $player_table_t

sequence player_seq
table player : player_table_t PRIMARY KEY Player
    , CONSTRAINT UniqName UNIQUE Username

type room_pass_t = [ Room = int, Pass = option string ]

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
  ++ rule_set_t
  ++ [ CurrentTurn = int
     , GameStarted = time
     , LastAction  = time
     , GameEnded   = option time ]

type game_table = $game_table_t

table game :
      game_table_t PRIMARY KEY (Room, Game)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)

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

table liberal :
      in_game_id_per_game_t
          CONSTRAINT HasPlayerInGame
          FOREIGN KEY (Room, Game, InGameId)
          REFERENCES player_in_game (Room, Game, InGameId)

table fascist :
      in_game_id_per_game_t
          CONSTRAINT HasPlayerInGame
          FOREIGN KEY (Room, Game, InGameId)
          REFERENCES player_in_game (Room, Game, InGameId)

table hitler :
      in_game_id_per_game_t
      CONSTRAINT UniqueHilterPerGame UNIQUE (Room, Game, InGameId)
    , CONSTRAINT HasPlayerInGame
      FOREIGN KEY (Room, Game, InGameId)
      REFERENCES player_in_game (Room, Game, InGameId)

type in_game_ordering_table_t = game_id_t ++ [ InGameId = int, Place = int ]

type in_game_ordering_table = $in_game_ordering_table_t

table in_game_ordering :
      in_game_ordering_table_t CONSTRAINT UniqueOrderingPerGame
                               UNIQUE (Room, Game, InGameId, Place)
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)
    , CONSTRAINT InGame FOREIGN KEY (Room, Game, InGameId)
                 REFERENCES player_in_game (Room, Game, InGameId)

type turn_id_t = game_id_t ++ [ Turn = int ]

type govt_state_t
  = [ President   = int
    , NextPres    = int
    , Chancellor  = int
    , RejectCount = int
    ]

type game_flow_t
  = [ ChancellorSelectionDone = bool
    ,                VoteDone = bool
    ,             DiscardDone = bool
    ,            EnactionDone = bool
    ,          ExecActionDone = bool
    ]

type deck_state_t
  = [ LiberalsInDraw = int, FascistsInDraw = int
    , LiberalsInDisc = int, FascistsInDisc = int
    , Fst = bool
    , Snd = bool
    , Trd = bool
    ]

type game_state_t
  = [ PresDisc        = int
    , ChanEnac        = int
    , LiberalPolicies = int
    , FascistPolicies = int
    ]

type turn_table_t = turn_id_t ++ govt_state_t ++ game_flow_t ++ deck_state_t ++ game_state_t

type turn_table = $turn_table_t

table turn :
      turn_table_t PRIMARY KEY (Room, Game, Turn)
    , CONSTRAINT UniquePresident  UNIQUE (Room, Game, Turn, President)
    , CONSTRAINT UniqueNextPres   UNIQUE (Room, Game, Turn, NextPres)
    , CONSTRAINT UniqueChancellor UNIQUE (Room, Game, Turn, Chancellor)
    , CONSTRAINT     PresNotDummy CHECK President <> 0
    , CONSTRAINT NextPresNotDummy CHECK  NextPres <> 0
    , CONSTRAINT ChancellorSelectedNotDummy
                 CHECK IF ChancellorSelectionDone THEN Chancellor <> 0 ELSE TRUE
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)
    , CONSTRAINT       PresIsPlayer FOREIGN KEY President  REFERENCES player (Player)
    , CONSTRAINT   NextPresIsPlayer FOREIGN KEY NextPres   REFERENCES player (Player)
    , CONSTRAINT ChancellorIsPlayer FOREIGN KEY Chancellor REFERENCES player (Player)
    , CONSTRAINT RejectCount CHECK RejectCount <= 3 AND RejectCount >= 0
    , CONSTRAINT AllowedDisc CHECK PresDisc >= 0 AND PresDisc <= 3
    , CONSTRAINT AllowedEnac CHECK ChanEnac >= 0 AND ChanEnac <= 3
    , CONSTRAINT VoteBeforeDisc  CHECK IF DiscardDone THEN VoteDone ELSE TRUE
    , CONSTRAINT DiscardNotDummy CHECK IF DiscardDone THEN PresDisc <> 0 ELSE TRUE
    , CONSTRAINT DiscardBeforeEnaction CHECK IF EnactionDone THEN DiscardDone ELSE TRUE
    , CONSTRAINT EnactionNotDummy CHECK IF EnactionDone THEN ChanEnac <> 0 ELSE TRUE
    , CONSTRAINT ExecActionAfterEnac CHECK IF ExecActionDone THEN EnactionDone ELSE TRUE
    , CONSTRAINT DrawDeckGTE3 CHECK LiberalsInDraw + FascistsInDraw >= 3
    , CONSTRAINT LibPolGTE0AndLTE5 CHECK LiberalPolicies >= 0 AND LiberalPolicies <= 5
    , CONSTRAINT FasPolGTE0AndLTE5 CHECK FascistPolicies >= 0 AND FascistPolicies <= 6
    , CONSTRAINT LiberalsUnchanging
          CHECK LiberalsInDraw + LiberalsInDisc + LiberalPolicies = {[number_of_liberal_policies]}
    , CONSTRAINT FascistsUnchanging
          CHECK FascistsInDraw + FascistsInDisc + FascistPolicies = {[number_of_fascist_policies]}

type vote_on_govt_table_t = turn_id_t ++ [ Player = int, Vote = bool ]

type vote_on_govt_table = $vote_on_govt_table_t

table vote_on_govt :
      vote_on_govt_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type president_action_table_t = turn_id_t ++ [ Target = int ]

table loyalty_investigation :
      president_action_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT TargetIsPlayer FOREIGN KEY Target REFERENCES player (Player)

table dead_player :
      president_action_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT TargetIsPlayer FOREIGN KEY Target REFERENCES player (Player)

type veto_table_t = turn_id_t ++ [ President = int, Chancellor = int ]

table veto :
      veto_table_t
      CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)


type chat_id_t = [ Chat = int ]

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

fun get_player_from_in_game_id (rt : room_table) (pid : int) : transaction {Player : int} =
    oneRow1 (SELECT player_in_game.Player
             FROM player_in_game
             WHERE player_in_game.Room = {[rt.Room]}
               AND player_in_game.Game = {[rt.CurrentGame]}
               AND player_in_game.InGameId = {[pid]})
