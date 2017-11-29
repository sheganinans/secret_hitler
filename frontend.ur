sequence room_seq
table room :
      { Room    : int
      , Private : bool
      } PRIMARY KEY Room

sequence game_seq
table game :
      { Game     : int
      , Room     : int
      , Pass     : string
      , AltRules : bool
      } PRIMARY KEY (Game, Room)
    , CONSTRAINT GameHasRoom FOREIGN KEY Room REFERENCES room (Room)

datatype action = NewGame

table player :
      { Id     : int
      , Game   : int
      , Room   : int
      , Nam    : string
      , Client : client
      , Chan   : channel action
      } PRIMARY KEY Id
    , CONSTRAINT PlayerHasRoom FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

type player = [ Game = int, Room = int, Player = int ]

table liberals :
      player
          CONSTRAINT LiberalsHavePlayer FOREIGN KEY Player REFERENCES player (Id)
    , CONSTRAINT LiberalsHaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table fascists :
      player
          CONSTRAINT FascistsHavePlayer FOREIGN KEY Player REFERENCES player (Id)
    , CONSTRAINT FascistsHaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table hitlers :
      player
          CONSTRAINT UniqueHilterPerGame UNIQUE (Game, Room, Player)
    , CONSTRAINT HitlersHavePlayer FOREIGN KEY Player REFERENCES player (Id)
    , CONSTRAINT HitlersHaveGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

fun game_view () : transaction xbody =
    return <xml></xml>
