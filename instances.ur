open Protocol
open Tables

val eq_room_player_relation : eq room_player_relation =
    mkEq (fn x y => x.Room = y.Room && x.Player = y.Player)

fun step_to_int (step : step) : int =
    case step of
        ChancellorSelectStep => 1
      | VoteStep             => 2
      | HitlerCheckStep      => 3
      | DiscardStep          => 4
      | EnactStep            => 5
      | ExecActionStep       => 6

val step_ord : ord step =
    mkOrd { Lt = fn a b => (step_to_int a) <  (step_to_int b)
          , Le = fn a b => (step_to_int a) <= (step_to_int b) }
