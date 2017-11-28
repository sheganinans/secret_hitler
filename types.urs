datatype side = Fascist | Liberal

val number_of_fascist_policies : int
val number_of_liberal_policies : int

type player_numbers_row = { Liberals : int, Fascists : int }

val player_numbers_table : list (int * player_numbers_row)

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
     , Dead : bool}

type game =
     { Hitler   :      player
     , Liberals : list player
     , Fascists : list player
     , Game     : int
     , Step     : step
     , AltRules : bool }

datatype action = NewGame

table games :
      { Id         : int
      , Nam        : option string
      , Pass       : string
      , LastAction : time}

table users :
      { Client : client
      , Chan   : channel action
      , Game   : int }
