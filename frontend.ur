sequence room_seq
table room :
      { Room    : int
      , Private : bool
      } PRIMARY KEY Room

type game_id = [ Game = int, Room = int]

type game_table =
     game_id ++
     [ Pass     = string
     , AltRules = bool
     ]

sequence game_seq
table game :
      game_table PRIMARY KEY (Game, Room)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)

datatype action = NewGame

type player_name_and_pass =
     [ Nam      = string
     , PassHash = string ]

type player_table =
     [ Player = int ] ++ player_name_and_pass

cookie username_and_pass : $player_name_and_pass

fun set_secure_cookie v =
    (* TODO: Make hash more secure with extra entropy from ENV_VAR. *)
    setCookie username_and_pass
              { Value   = v
              , Expires = None
              , Secure  = True }

sequence player_seq
table player : player_table PRIMARY KEY Player

type player_id_per_game =
     [ Player = int ] ++ game_id

type player_connection =
     player_id_per_game ++
     [ Client = client
     , Chan   = channel action
     ]

table player_in_game :
      player_connection PRIMARY KEY (Game, Room, Player)
    , CONSTRAINT HasGlobalId FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasRoom FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table liberals :
      player_id_per_game
          CONSTRAINT HavePlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table fascists :
      player_id_per_game
          CONSTRAINT HavePlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table hitlers :
      player_id_per_game
          CONSTRAINT UniqueHilterPerGame UNIQUE (Game, Room, Player)
    , CONSTRAINT HavePlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

type turn_id =
     game_id ++ [ Turn = int ]

type turn_table =
     turn_id ++ [ President = int ]

table turn :
      turn_table PRIMARY KEY (Game, Room, Turn)
    , CONSTRAINT HaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)
    , CONSTRAINT IsPlayer FOREIGN KEY President REFERENCES player (Player)

type deck_state_table =
     turn_id ++
     [ LiberalsInDraw = int, FascistsInDraw = int
     , LiberalsInDisc = int, FascistsInDisc = int
     , Fst = bool
     , Snd = bool
     , Trd = bool ]

table deck_state :
      deck_state_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
    , CONSTRAINT DrawDeckGTE3 CHECK LiberalsInDraw + FascistsInDraw >= 3

type game_state_table =
     turn_id ++
     [ LiberalPolicies = int, FascistPolicies = int ]

table game_state :
      game_state_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
    , CONSTRAINT LibPolGTE0 CHECK LiberalPolicies >= 0
    , CONSTRAINT FasPolGTE0 CHECK FascistPolicies >= 0

type chancellor_selection_table =
     turn_id ++ [ Chancellor = int ]

table chancellor_selection :
      chancellor_selection_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
    , CONSTRAINT IsPlayer FOREIGN KEY Chancellor REFERENCES player (Player)

type vote_on_govt_table =
      turn_id ++ [ Player = int, Vote = bool ]

table vote_on_govt :
      vote_on_govt_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT IsPlayer FOREIGN KEY Player REFERENCES player (Player)

type dead_players_table =
      turn_id ++ [ Player = int ]

table dead_players :
      dead_players_table
          CONSTRAINT DiedOnTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT IsPlayer FOREIGN KEY Player REFERENCES player (Player)

fun game_view () : transaction xbody =
    return <xml></xml>
