open Types

(* In game actions *)
datatype vote
  = Ya
  | Nein
  | UnVote

datatype president
  = ChooseChancellor     of int
  | PresidentDiscardCard of int
  | InvestigateLoyalty   of int
  | CallSpecialElection  of int
  | ExecutePlayer        of int
  | PresidentVeto

datatype chancellor
  = EnactPolicy of int
  | ProposeVeto

datatype in_game
  =      VoterAction of vote
  |  PresidentAction of president
  | ChancellorAction of chancellor

(* In room actions *)
datatype request
  = ReqInGame
  | ReqRuleSet
  | ReqPublicGameState

datatype set
  = SetNickName of string

datatype mod
  = Ban of int
  | Kick of { Player : int, Till : time }

datatype in_room
  = Req of request
  | Set of set
  | Mod of mod


(* Server response *)
datatype president_response
  = DiscardCard          of side * side * side
  | LoyaltyInvestigation of { Player : int, Side : side }
  | PolicyPeek           of side * side * side
  | ExecutionPowerGranted
  | VetoProposed

datatype chancellor_response = Policies of side * side

type public_game_state
  = { President       : int
    , Chancellor      : option int
    , FascistPolicies : int
    , LiberalPolicies : int
    }

type game_end_state
  = { Winners  : side
    , Hitler   :      int
    , Liberals : list int
    , Fascists : list int
    , Dead     : list int
    , Start    : time
    }

datatype new_govt = Passed | Failed | InChaos

datatype game_role
  = Liberal
  | Hitler
  | Fascist of { Hitler : int, Fascists : list int }
  | Watcher

type game_state_init
  = { PublicGameState   :      public_game_state
    , History           : list public_game_state
    , ChatHistory       : list chat_contents
    , Players           : list { Player : int, Username : string }
    , GameRole          : game_role
    , KnownAffiliations : list { Player : int, Side : side }
    }

datatype general_response
  = Chat             of chat_contents
  | RuleSet          of rule_set
  | GameStateInit    of game_state_init
  | PresidentNow     of int
  | ChancellorChosen of int
  | NewGovt          of new_govt
  | PolicyPassed     of side
  | Punished
  | PlayersPunished  of list int
  | Executed
  | PlayerExecuted   of int
  | Veto
  | GameEndState     of game_end_state

datatype turn_role = Voter | President | Chancellor

datatype in_game_response
  = TurnRole      of turn_role
  |    GeneralRsp of    general_response
  |  PresidentRsp of  president_response
  | ChancellorRsp of chancellor_response
