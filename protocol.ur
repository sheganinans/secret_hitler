open Types
open Tables

(* Player Actions *)
datatype voter_action
  = Ya
  | Nein
  | UnVote

datatype card = Fst | Snd | Trd

datatype president_action
  = ChooseChancellor    of int
  | DiscardCard         of card
  | InvestigateLoyalty  of int
  | CallSpecialElection of int
  | ExecutePlayer       of int
  | Veto

datatype chancellor_action
  = EnactPolicy of card
  | ProposeVeto

datatype player_action
  = Voter      of      voter_action
  | President  of  president_action
  | Chancellor of chancellor_action

(* Server Responses *)
datatype president_response
  = ChooseYourChancellor
  | DiscardCard          of (side * side * side)
  | LoyaltyInvestigation of (int * side)
  | PolicyPeek           of (side * side * side)
  | ExecutionPowerGranted
  | VetoProposed

datatype chancellor_response = Policies of (side * side)

type game_end_state
  = { Winners  : side
    , Hitler   :      int
    , Liberals : list int
    , Fascists : list int }

type current_game_state
  = { President       : int
    , Chancellor      : option int
    , FascistPolicies : int
    , LiberalPolicies : int
    , Players         : list { Player : int, Username : string }
    , ChatHistory     : list { Player : int, When : time, Msg : string }
    }

datatype new_govt = Passed | Failed | InChaos

datatype game_state_info
  = Liberal
  | Hitler
  | Fascist of { Hitler : int, Fascists : list int }

type game_state_init
  = { CurrentGameState : current_game_state
    , GameStateInfo    : game_state_info
    }

datatype general_response
  = RuleSet          of rule_set
  | GameStateInit    of game_state_init
  | PresidentNow     of int
  | ChancellorChosen of int
  | NewGovt          of new_govt
  | Punished
  | Executed
  | GameEndState     of game_end_state

datatype watcher_response
  = RuleSet          of rule_set
  | CurrentGameState of current_game_state
  | PresidentNow     of int
  | ChancellorChosen of int
  | NewGovt          of new_govt
  | PolicyPassed     of side
  | PlayersPunished  of list int
  | PlayerExecuted   of int
  | Veto
  | GameEndState     of game_end_state

datatype turn_role = Voter | President | Chancellor

datatype server_response
  = TurnRole    of turn_role
  | General     of    general_response
  | President   of  president_response
  | Chancellor  of chancellor_response
  | Watcher     of    watcher_response
