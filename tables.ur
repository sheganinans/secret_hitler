open Types

type player_table =
     [ Player = int ] ++ player_name_and_pass

sequence player_seq
table player : $player_table PRIMARY KEY Player
    , CONSTRAINT UniqName UNIQUE Username

type room_pass =
    [ Room = int
    , Pass = option int ]

type room_table =
    room_pass
  ++ [ Nam  = string
     , OwnedBy = int
     , CurrentGame = int ]

sequence room_seq
table room :
      $room_table PRIMARY KEY Room
    , CONSTRAINT OwnedByPlayer FOREIGN KEY OwnedBy REFERENCES player (Player)

type room_player_relation =
      [ Room   = int
      , Player = int
      ]

table room_user :
      $room_player_relation PRIMARY KEY (Room, Player)
    , CONSTRAINT   RoomExists FOREIGN KEY Room   REFERENCES room   (Room)
    , CONSTRAINT PlayerExists FOREIGN KEY Player REFERENCES player (Player)

table mod :
      $room_player_relation
       CONSTRAINT IsRoomPlayerRelation
       FOREIGN KEY (Room, Player)
       REFERENCES room_user (Room, Player)

table ban :
      $room_player_relation
       CONSTRAINT IsRoomPlayerRelation
       FOREIGN KEY (Room, Player)
       REFERENCES room_user (Room, Player)

type game_id = [ Game = int, Room = int ]

type game_time_table =
     [ ChanNomTime = float
     , GovVoteTime = float
     , PresDisTime = float
     , ChanEnaTime = float
     , ExecActTime = float
     ]

type game_table =
     game_id
    ++ game_time_table
    ++ [ CurrentTurn = option int ]

sequence game_seq
table game :
      $game_table PRIMARY KEY (Game, Room)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)

type player_id_per_game =
     [ Player = int ] ++ game_id

type player_connection =
     player_id_per_game ++
     [ Client = client
     , Chan   = channel action
     ]

table player_in_game :
      $player_connection PRIMARY KEY (Game, Room, Player)
    , CONSTRAINT HasGlobalId FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasRoom FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table liberal :
      $player_id_per_game
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table fascist :
      $player_id_per_game
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table hitler :
      $player_id_per_game
          CONSTRAINT UniqueHilterPerGame UNIQUE (Game, Room, Player)
    , CONSTRAINT HavePlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

type turn_id =
     game_id ++ [ Turn = int ]

type govt_state =
     [ President  = int
     , NextPres   = int
     , Chancellor = int
     ]

type deck_state =
     [ LiberalsInDraw = int, FascistsInDraw = int
     , LiberalsInDisc = int, FascistsInDisc = int
     , Fst = bool
     , Snd = bool
     , Trd = bool ]

type game_state =
     [ LiberalPolicies = int, FascistPolicies = int ]

type turn_table =
     turn_id    ++
     govt_state ++
     deck_state ++
     game_state

table turn :
      $turn_table PRIMARY KEY (Game, Room, Turn)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)
    , CONSTRAINT       PresIsPlayer FOREIGN KEY President  REFERENCES player (Player)
    , CONSTRAINT   NextPresIsPlayer FOREIGN KEY NextPres   REFERENCES player (Player)
    , CONSTRAINT ChancellorIsPlayer FOREIGN KEY Chancellor REFERENCES player (Player)
    , CONSTRAINT DrawDeckGTE3 CHECK LiberalsInDraw + FascistsInDraw >= 3
    , CONSTRAINT LibPolGTE0 CHECK LiberalPolicies >= 0
    , CONSTRAINT FasPolGTE0 CHECK FascistPolicies >= 0

type player_action_id =
     turn_id ++ [ Player = int ]

type vote_on_govt_table =
     player_action_id ++ [ Vote = bool ]

table vote_on_govt :
      $vote_on_govt_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type loyalty_investigation_table =
     player_action_id

table loyalty_investigation :
      $loyalty_investigation_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type dead_player_table =
      player_action_id

table dead_player :
      $dead_player_table
          CONSTRAINT DiedOnTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT IsPlayer FOREIGN KEY Player REFERENCES player (Player)

type veto_table =
     turn_id

table veto :
      $veto_table
      CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
