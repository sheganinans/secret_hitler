datatype action = NewGame

type player_name_and_pass =
     [ Username = string
     , PassHash = string ]

cookie username_and_pass : $player_name_and_pass

fun set_username_cookie (v : $player_name_and_pass) =
    setCookie username_and_pass
              { Value   = v
              , Expires = None
              , Secure  = True }

type player_table =
     [ Player = int ] ++ player_name_and_pass

sequence player_seq
table player : player_table PRIMARY KEY Player
    , CONSTRAINT UniqName UNIQUE Username

type room_link_pass =
    [ Room = int
    , Link = string
    , Pass = string ]

type room_table = [ Nam  = string, OwnedBy = int ] ++ room_link_pass

sequence room_seq
table room :
      room_table PRIMARY KEY Room
    , CONSTRAINT UniqueRoomLink UNIQUE Link
    , CONSTRAINT OwnedByPlayer FOREIGN KEY OwnedBy REFERENCES player (Player)

table bans :
      { Room   : int
      , Player : int
      } CONSTRAINT RoomExists FOREIGN KEY Room   REFERENCES room   (Room)
    , CONSTRAINT PlayerExists FOREIGN KEY Player REFERENCES player (Player)

type game_id = [ Game = int, Room = int ]

type game_table =
     game_id ++
     [ CurrentTurn = int
     , ChanNomTime = int
     , GovVoteTime = int
     , PresDisTime = int
     , ChanEnaTime = int
     , ExecActTime = int
     ]

sequence game_seq
table game :
      game_table PRIMARY KEY (Game, Room)
    , CONSTRAINT HasRoom FOREIGN KEY Room REFERENCES room (Room)

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

table liberal :
      player_id_per_game
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table fascist :
      player_id_per_game
          CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)
    , CONSTRAINT HasGame FOREIGN KEY (Game, Room) REFERENCES game (Game, Room)

table hitler :
      player_id_per_game
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
      turn_table PRIMARY KEY (Game, Room, Turn)
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
      vote_on_govt_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type loyalty_investigation_table =
     player_action_id

table loyalty_investigation :
      loyalty_investigation_table
          CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT HasPlayer FOREIGN KEY Player REFERENCES player (Player)

type dead_player_table =
      player_action_id

table dead_player :
      dead_player_table
          CONSTRAINT DiedOnTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)
      , CONSTRAINT IsPlayer FOREIGN KEY Player REFERENCES player (Player)

type veto_table =
     turn_id

table veto :
      veto_table
      CONSTRAINT HasTurn FOREIGN KEY (Game, Room, Turn) REFERENCES turn (Game, Room, Turn)

fun game_loop (initial_state : Types.game) =
    let fun loop_it ((me,chan,state) : client * channel action * source Types.game) : transaction {} =
            let fun loop () =
                    sleep 1;
                    loop ()
            in loop () end
    in me <- self;
       chan <- channel;
       (*dml (INSERT INTO users (Client, Chan, Game) VALUES ({[me]}, {[chan]}, {[initial_state.Game]}));*)

       state <- source initial_state;
       loop_it (me,chan,state) end

fun view_room (link : string) =
    return <xml>{[link]}</xml>

fun join_room () : transaction page =
    return <xml>Join Room</xml>

fun main_menu () =
    un_pw_opt <- getCookie username_and_pass;
    case un_pw_opt of
        None   => signup_page ()
      | Some _ =>
        return <xml><body><table>
          <tr><td><div link={new_room ()}>New Room</div></td></tr>
          <tr><td>Join Room</td></tr>
        </table></body></xml>

and signup_page () : transaction page =
    return <xml><body><form>
      <table>
        <tr>Signup!</tr>
        <tr><th>Username:</th><td><textbox{#Username}/></td></tr>
        <tr><th>Password:</th><td><password{#PassHash}/></td></tr>
        <tr><th/><td><submit action={submit_signup}/></td></tr>
      </table></form></body></xml>

and submit_signup (signup : $player_name_and_pass) : transaction page =
    let val pw_hs = Auth.basic_password_hash signup.PassHash
    in set_username_cookie { Username = signup.Username, PassHash = pw_hs };

       player_id <- nextval player_seq;

       dml (INSERT INTO player (Player, Username, PassHash)
            VALUES ({[player_id]}, {[signup.Username]}, {[pw_hs]}));

       main_menu ()
    end

and new_room () : transaction page =
    c1 <- getCookie username_and_pass;
    (case c1 of
         None => debug "4"
       | Some c => debug c.PassHash);
    return <xml><body><form>
      <table>
        <tr>New Room</tr>
        <tr><th>Name</th><td><textbox{#Nam}/></td></tr>
        <tr><th>Private?</th><td><checkbox{#Private}/></td></tr>
        <tr><th/><td><submit action={submit_new_room}/></td></tr>
      </table>
    </form></body></xml>

and submit_new_room room_form =
    c1 <- getCookie username_and_pass;
    (case c1 of
         None => debug "5"
       | Some c => debug c.PassHash);
    room_id <- nextval room_seq;
    un_cookie <- getCookie username_and_pass;
    let val link = Auth.basic_password_hash (show room_id)
        val pass = if room_form.Private then Auth.basic_password_hash link else ""
    in  case un_cookie of
            None => return <xml>Err1</xml>
          | Some un_pw =>
            u <- oneOrNoRows1 (SELECT player.Player
                               FROM player
                               WHERE player.Username = {[un_pw.Username]});
            case u of
                None => return <xml>Err2</xml>
              | Some u =>
                dml (INSERT INTO room (Room, OwnedBy, Nam, Link, Pass)
                     VALUES ({[room_id]}, {[u.Player]}, {[room_form.Nam]}, {[link]}, {[pass]}));
                view_room link
    end
