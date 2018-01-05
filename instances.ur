open Protocol
open Tables

val eq_room_player_relation : eq room_player_relation =
    mkEq (fn x y => x.Room = y.Room && x.Player = y.Player)
