open Types

datatype in_game
  =      VoterAction of option bool
  |  PresidentAction of president
  | ChancellorAction of chancellor
  |       ChatAction of chat_contents

and president
  = StandardAction     of standard_action
  | ExecutiveActionMsg of executive_action

and standard_action
  = ChooseChancellor     of int
  | PresidentDiscardCard of int

and executive_action
  = InvestigateLoyaltyAct  of int
  | CallSpecialElectionAct of int
  | ExecutePlayer          of int
  | PresidentVeto

and chancellor
  = EnactPolicy of int
  | ProposeVeto


(* In room actions *)
datatype in_room
  = Req of request
  | Set of set
  | Mod of mod

and request
  = ReqInGame
  | ReqRuleSet
  | ReqPublicGameState

and set
  = SetNickName of string

and mod
  = Ban of int
  | Kick of { Player : int, Till : time }

type public_game_state
  = { CurrentTurn :      govt_state
    , GameHistory : list govt_state
    , ChatHistory : list chat_contents
    , Players     : list { Player : int, Username : string }
    }

type game_end_state_t
  = [ Winners  = side
    , Hitler   =      int
    , Liberals = list int
    , Fascists = list int
    , Dead     = list { Turn : int, Place : int }
    , Start    = time
    , End      = time
    ]

type game_end_state = $game_end_state_t

datatype game_role
  = Liberal
  | Hitler
  | Fascist of { Hitler : int, Fascists : list int }
  | Watcher

type private_game_state
  = { PublicGameState   : public_game_state
    , GameRole          : game_role
    , KnownAffiliations : list { Player : int, Side : side }
    , Top3CardsInDraw   : list { Turn : int, Cards : bool * bool * bool }
    }

datatype turn_role = Voter | President | Chancellor

datatype new_govt = Passed | Failed | InChaos

(* Server response *)
datatype in_game_response
  = TurnRole      of turn_role
  |    GeneralRsp of    general_response
  |  PresidentRsp of  president_response
  | ChancellorRsp of chancellor_response

and general_response
  = Chat             of chat_contents
  | RuleSet          of rule_set
  | PublicGameState  of public_game_state
  | PresidentNow     of int
  | ChancellorChosen of int
  | NewGovt          of new_govt
  | PresidentDiscard
  | PolicyPassed     of side
  | PlayersPunished  of list int
  | PlayerExecuted   of int
  | VetoEnacted
  | GameEndState     of game_end_state

and president_response
  = DiscardCard          of side * side * side
  | LoyaltyInvestigation of { Player : int, Side : side }
  | PolicyPeek           of side * side * side
  | ExecutionPowerGranted
  | VetoProposed

and chancellor_response = Policies of side * side
