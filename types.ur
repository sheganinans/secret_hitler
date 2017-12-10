(* Rulebook types *)

datatype side = Fascist | Liberal

val number_of_fascist_policies = 11
val number_of_liberal_policies = 6

type player_numbers_row = { Liberals : int, Fascists : int }

val player_numbers_table : list (int * player_numbers_row) =
    (5 , { Liberals = 3, Fascists = 1 }) ::
    (6 , { Liberals = 4, Fascists = 1 }) ::
    (7 , { Liberals = 4, Fascists = 2 }) ::
    (8 , { Liberals = 5, Fascists = 2 }) ::
    (9 , { Liberals = 5, Fascists = 3 }) ::
    (10, { Liberals = 6, Fascists = 3 }) :: []

datatype election_step
  = PassThePresidentialCandidacy
  | NominateAChancellor (* Check eligibility *)
  | VoteOnTheGovernment (* Election Tracker *)
  | NewGovernment       (* Check if chancelor is Hitler *)

datatype legislative_step
  = PresidentialDiscard
  | ChancellorEnaction

datatype executive_action
  = InvestigateLoyalty
  | CallSpecialElection
  | PolicyPeek
  | Execution
  | Veto

datatype step
  = Election           of election_step
  | LegislativeSession of legislative_step
  | ExecutiveAction    of executive_action

(* Game state types *)

type player =
     { Id   : int
     , Nam  : string
     , Dead : bool }

type game =
     { Hitler   :      player
     , Liberals : list player
     , Fascists : list player
     , Game     : int
     , Step     : step
     , AltRules : bool }

datatype action = NewGame

(* Auth Types *)

type player_name_and_pass =
     [ Username = string
     , PassHash = string ]

(* Result Type *)

datatype result ok err =
     | Ok  of ok
     | Err of err
