open Protocol
open Tables

val eq_room_player_relation : eq room_player_relation =
    mkEq (fn x y => x.Room = y.Room && x.Player = y.Player)

fun step_to_int (step : step) : int =
    case step of
        ChancellorSelectStep => 10
      | VoteStep             => 20
      | DiscardStep          => 30
      | EnactStep            => 40
      | ExecActionStep       => 50

val step_ord : ord step =
    mkOrd { Lt = fn a b => step_to_int a <  step_to_int b
          , Le = fn a b => step_to_int a <= step_to_int b }

val step_eq : eq step = mkEq (fn a b => step_to_int a = step_to_int b)
