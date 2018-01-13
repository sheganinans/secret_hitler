(* Rulebook types *)

datatype side = Fascist | Liberal

fun side_from_bool (b : bool) : side = if b then Liberal else Fascist
fun bool_from_side (s : side) : bool = case s of
                                           Liberal => True
                                         | Fascist => False

val side_eq : eq side = mkEq (fn a b => bool_from_side a = bool_from_side b)

val number_of_fascist_policies = 11
val number_of_liberal_policies = 6

type player_numbers_row = { Liberals : int, Fascists : int }

val player_numbers_table : list (int * player_numbers_row) =
    (2 , { Liberals = 1, Fascists = 0 }) :: (* TESTING *)
    (5 , { Liberals = 3, Fascists = 1 }) ::
    (6 , { Liberals = 4, Fascists = 1 }) ::
    (7 , { Liberals = 4, Fascists = 2 }) ::
    (8 , { Liberals = 5, Fascists = 2 }) ::
    (9 , { Liberals = 5, Fascists = 3 }) ::
    (10, { Liberals = 6, Fascists = 3 }) :: []

(* Table Types *)

type rule_set_t
  = [ TimedGame   = bool
    , KillPlayer  = bool
    , ChanNomTime = float
    , GovVoteTime = float
    , PresDisTime = float
    , ChanEnaTime = float
    , ExecActTime = float
    ]

type rule_set = $rule_set_t

type govt_state_t
  = [ President       = int
    , Chancellor      = int
    , LiberalPolicies = int
    , FascistPolicies = int
    , RejectCount     = int
    ]

type govt_state = $govt_state_t

type chat_contents_t = [ Player = int, When = time, Text = string ]

type chat_contents = $chat_contents_t

datatype step
  = ChancellorSelectStep
  | VoteStep
  | DiscardStep
  | EnactStep
  | ExecActionStep

datatype card = Fst | Snd | Trd

datatype action
  = RoomAction of room_action
  | GameAction of game_action
and room_action
  = SetRuleSet
  | StartGame
and game_action
  = Vote  of option bool
  | Timed of timed_action
and timed_action
  = ChooseChancellor of int
  | DiscardPolicy    of card
  |   EnactPolicy    of card
  | ExecutiveAction  of exec_action
and exec_action
  = InvestigateLoyalty  of int
  | CallSpecialElection of int
  | ExecutePlayer       of int
  | ProposeVeto
  | RejectVeto
  | Veto

datatype capability_arg
  = RuleSetArg of rule_set
  |    VoteArg of option bool

type current_decks_t
  = [ LiberalsInDraw = int, FascistsInDraw = int
    , LiberalsInDisc = int, FascistsInDisc = int
    ]

(* Auth Types *)

type player_name_and_pass_t =
     [ Username = string
     , PassHash = string ]

type player_name_and_pass = $player_name_and_pass_t

type player_id_and_username = { Player : int, Username : string }

(* Result Type *)

datatype result ok err
  = Ok  of ok
  | Err of err

(* Type Synonyms *)

type make_form = xml [MakeForm, Dyn, Body] [] []
