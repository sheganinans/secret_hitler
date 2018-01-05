open Protocol
open Tables

val eq_room_player_relation : eq room_player_relation =
    mkEq (fn x y => x.Room = y.Room && x.Player = y.Player)

val eq_vote : eq vote =
    mkEq (fn x y => case (x,y) of
                        (    Ya,     Ya) => True
                      | (  Nein,   Nein) => True
                      | (UnVote, UnVote) => True
                      |                _ => False)
