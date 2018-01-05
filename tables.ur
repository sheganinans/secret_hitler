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
      room_player_relation_t
       CONSTRAINT IsRoomPlayerRelation
       FOREIGN KEY (Room, Player)
       REFERENCES room_player (Room, Player)

table ban :
      room_player_relation_t
       CONSTRAINT IsRoomPlayerRelation
       FOREIGN KEY (Room, Player)
       REFERENCES room_player (Room, Player)

type kick_table_t = room_player_relation_t ++ [ Till = time ]

type kick_table = $kick_table_t

table kick :
      kick_table_t
       CONSTRAINT IsRoomPlayerRelation
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

type player_id_per_game_t = [ Player = int ] ++ game_id_t

type player_id_per_game = $player_id_per_game_t

type player_connection_t
  = player_id_per_game_t
  ++ [ Client   = client
     , Chan     = channel Protocol.in_game_response
     , Watching = bool
     , InGameId = option int
     ]

type player_connection = $player_connection_t

table player_in_game :
      player_connection_t PRIMARY KEY (Room, Game, Player)
    , CONSTRAINT HasGlobalId FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasRoom FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)

table liberal :
      player_id_per_game_t
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)

table fascist :
      player_id_per_game_t
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)

table hitler :
      player_id_per_game_t
          CONSTRAINT UniqueHilterPerGame UNIQUE (Room, Game, Player)
    , CONSTRAINT HavePlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HaveGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)

type turn_id_t = game_id_t ++ [ Turn = int ]

type govt_state_t
  = [ President   = int
    , NextPres    = int
    , Chancellor  = option int
    , RejectCount = int
    , VoteDone    = bool
    ]

type deck_state_t
  = [ LiberalsInDraw = int, FascistsInDraw = int
    , LiberalsInDisc = int, FascistsInDisc = int
    , Fst = bool
    , Snd = bool
    , Trd = bool
    , PresDisc = int
    , ChanEnac = int
    ]

type game_state_t
  = [ LiberalPolicies = int
    , FascistPolicies = int
    ]

type turn_table_t = turn_id_t ++ govt_state_t ++ deck_state_t ++ game_state_t

type turn_table = $turn_table_t

table turn :
      turn_table_t PRIMARY KEY (Room, Game, Turn)
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)
    , CONSTRAINT       PresIsPlayer FOREIGN KEY President  REFERENCES player (Player)
    , CONSTRAINT   NextPresIsPlayer FOREIGN KEY NextPres   REFERENCES player (Player)
    , CONSTRAINT ChancellorIsPlayer FOREIGN KEY Chancellor REFERENCES player (Player)
    , CONSTRAINT DrawDeckGTE3 CHECK LiberalsInDraw + FascistsInDraw >= 3
    , CONSTRAINT AllowedDisc CHECK PresDisc >= 0 AND PresDisc <= 3
    , CONSTRAINT AllowedEnac CHECK ChanEnac >= 0 AND ChanEnac <= 3
    , CONSTRAINT LibPolGTE0 CHECK LiberalPolicies >= 0
    , CONSTRAINT FasPolGTE0 CHECK FascistPolicies >= 0
    , CONSTRAINT LiberalsUnchanging
                 CHECK LiberalsInDraw
                     + LiberalsInDisc
                     + LiberalPolicies
                     = {[number_of_liberal_policies]}
    , CONSTRAINT FascistsUnchanging
                  CHECK FascistsInDraw
                      + FascistsInDisc
                      + FascistPolicies
                      = {[number_of_fascist_policies]}

type vote_on_govt_table_t = turn_id_t ++ [ Player = int, Vote = bool ]

type vote_on_govt_table = $vote_on_govt_table_t

table vote_on_govt :
      vote_on_govt_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type president_action_id_t = turn_id_t ++ [ Target = int ]

type loyalty_investigation_table_t = president_action_id_t

table loyalty_investigation :
      loyalty_investigation_table_t
          CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Target REFERENCES player (Player)

type dead_player_table_t = president_action_id_t

table dead_player :
      dead_player_table_t
          CONSTRAINT DiedOnTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)
      , CONSTRAINT IsPlayer FOREIGN KEY Target REFERENCES player (Player)

type veto_table_t = turn_id_t

table veto :
      veto_table_t
      CONSTRAINT HasTurn FOREIGN KEY (Room, Game, Turn) REFERENCES turn (Room, Game, Turn)

type chat_table_t = game_id_t ++ chat_contents_t

type chat_table = $chat_table_t

table chat :
      chat_table_t PRIMARY KEY (Room, Game, Player, When)
    , CONSTRAINT HasGame FOREIGN KEY (Room, Game) REFERENCES game (Room, Game)
    , CONSTRAINT HasPlayer FOREIGN KEY (Room, Game, Player)
                 REFERENCES player_in_game (Room, Game, Player)

fun get_player_from_in_game_id (rt : room_table) (pid : int) : transaction {Player : int} =
    oneRow1 (SELECT player_in_game.Player
             FROM player_in_game
             WHERE player_in_game.Room = {[rt.Room]}
               AND player_in_game.Game = {[rt.CurrentGame]}
               AND player_in_game.InGameId = {[Some pid]})
