open Types

type player_table_t =
     [ Player = int ] ++ player_name_and_pass

type player_table = $player_table_t

sequence player_seq
table player : player_table_t PRIMARY KEY Player
    , CONSTRAINT UniqName UNIQUE Username

type room_pass_t =
    [ Room = int
    , Pass = option int ]

type room_table_t =
    room_pass_t
  ++ [ Nam  = string
     , OwnedBy = int
     , CurrentGame = int ]

type room_table = $room_table_t

sequence room_seq
table room :
      room_table_t PRIMARY KEY Room
    , CONSTRAINT OwnedByPlayer FOREIGN KEY OwnedBy REFERENCES player (Player)

type room_player_relation_t =
      [ Room   = int
      , Player = int
      ]

type room_player_relation = $room_player_relation_t

table room_user :
      room_player_relation_t PRIMARY KEY (Room, Player)
    , CONSTRAINT   RoomExists FOREIGN KEY Room   REFERENCES room   (Room)
    , CONSTRAINT PlayerExists FOREIGN KEY Player REFERENCES player (Player)

table mod :
      room_player_relation_t
       CONSTRAINT IsRoomPlayerRelation
       FOREIGN KEY (Room, Player)
       REFERENCES room_user (Room, Player)

table ban :
      room_player_relation_t
       CONSTRAINT IsRoomPlayerRelation
       FOREIGN KEY (Room, Player)
       REFERENCES room_user (Room, Player)

type game_id_t = [ Game = int, Room = int ]

type game_time_table_t =
     [ ChanNomTime = float
     , GovVoteTime = float
     , PresDisTime = float
     , ChanEnaTime = float
     , ExecActTime = float
     ]

type game_time_table = $game_time_table_t

type game_table_t =
     game_id_t
    ++ game_time_table_t
    ++ [ CurrentTurn = option int ]

type game_table = $game_table_t

sequence game_seq
table game :
      game_table_t PRIMARY KEY (Game, Room)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)

type player_id_per_game_t =
     [ Player = int ] ++ game_id_t

type player_id_per_game = $player_id_per_game_t

type player_connection_t =
     player_id_per_game_t ++
     [ Client = client
     , Chan   = channel action
     ]

type player_connection = $player_connection_t

table player_in_game :
      player_connection_t PRIMARY KEY (Game, Room, Player)
    , CONSTRAINT HasGlobalId FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasRoom FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table liberal :
      player_id_per_game_t
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table fascist :
      player_id_per_game_t
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table hitler :
      player_id_per_game_t
          CONSTRAINT UniqueHilterPerGame UNIQUE (Game, Room, Player)
    , CONSTRAINT HavePlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

type turn_id_t =
     game_id_t ++ [ Turn = int ]

type govt_state_t =
     [ President  = int
     , NextPres   = int
     , Chancellor = int
     ]

type deck_state_t =
     [ LiberalsInDraw = int, FascistsInDraw = int
     , LiberalsInDisc = int, FascistsInDisc = int
     , Fst = bool
     , Snd = bool
     , Trd = bool ]

type game_state_t =
     [ LiberalPolicies = int, FascistPolicies = int ]

type turn_table_t =
     turn_id_t    ++
     govt_state_t ++
     deck_state_t ++
     game_state_t

type turn_table = $turn_table_t

table turn :
      turn_table_t PRIMARY KEY (Game, Room, Turn)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)
    , CONSTRAINT       PresIsPlayer FOREIGN KEY President  REFERENCES player (Player)
    , CONSTRAINT   NextPresIsPlayer FOREIGN KEY NextPres   REFERENCES player (Player)
    , CONSTRAINT ChancellorIsPlayer FOREIGN KEY Chancellor REFERENCES player (Player)
    , CONSTRAINT DrawDeckGTE3 CHECK LiberalsInDraw + FascistsInDraw >= 3
    , CONSTRAINT LibPolGTE0 CHECK LiberalPolicies >= 0
    , CONSTRAINT FasPolGTE0 CHECK FascistPolicies >= 0

type player_action_id_t =
     turn_id_t ++ [ Player = int ]

type vote_on_govt_table_t =
     player_action_id_t ++ [ Vote = bool ]

type vote_on_govt_table = $vote_on_govt_table_t

table vote_on_govt :
      vote_on_govt_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type loyalty_investigation_table_t =
     player_action_id_t

table loyalty_investigation :
      loyalty_investigation_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type dead_player_table_t =
      player_action_id_t

table dead_player :
      dead_player_table_t
          CONSTRAINT DiedOnTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT IsPlayer FOREIGN KEY Player REFERENCES player (Player)

type veto_table_t =
     turn_id_t

table veto :
      veto_table_t
      CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
