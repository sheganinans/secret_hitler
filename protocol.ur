open Types

type public_game_state_t
  = [ CurrentStep = step
    , CurrentTurn =      govt_state
    , GameHistory = list govt_state
(*    , ChatHistory = list chat_contents*)
    , Players     = list { Place    : int
                         , Username : string
                         , Alive    : bool
                         }
    ]

type public_game_state = $public_game_state_t

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

type affiliations = list { Player : int, Side : side }

type private_game_state_t
  = [ PublicGameState   = public_game_state
    , GameRole          = game_role
    , KnownAffiliations = affiliations
    , Top3CardsInDraw   = list { Turn : int, Cards : side * side * side }
    ]

type private_game_state = $private_game_state_t

datatype turn_role = Voter | President | Chancellor

datatype new_govt = Passed | Failed | InChaos

type side_cap = { Capability : int, Side : side }

type vote_notif = { Place : int, Vote : bool }

(* Server response *)
datatype in_game_response
  = PublicRsp  of  public_response
  | PrivateRsp of private_response

and public_response
  = PlayersOnTable   of list { InGameId : int, Username : string }
  | NewPlayer        of { InGameId : int, Username : string }
  | PlayerLeaves     of int
(*  | Chat             of chat_contents (* Chatting is a room level funtion, not game level. *) *)
  | RuleSet          of rule_set
  | PublicGameState  of public_game_state
  | NewTurn          of govt_state
  | TurnRole         of turn_role
  | ChancellorChosen of int
  | VoteNotif        of vote_notif
  | NewGovt          of new_govt
  | PresidentDiscard
  | PolicyPassed     of side
  | PlayersPunished  of list int
  | ExecutionPower
  | PlayerExecuted   of int
  | VetoEnacted
  | GameEndState     of game_end_state

and private_response
  =      VoterRsp of      voter_response
  |  PresidentRsp of  president_response
  | ChancellorRsp of chancellor_response
  |    FascistRsp of    fascist_response
  |        ModRsp of        mod_response

and voter_response
    = VoteCapability of int

and president_response
  = SelectCandidate of list { Nam : string, Cap : int }
  | DiscardCard     of side_cap * side_cap * side_cap
  | LoyaltyInvestig of { Place : int, Side : side }
  | PolicyPeek      of side * side * side
  | VetoProposed

and chancellor_response
  = Policies of { One     : side_cap
                , Two     : side_cap
                , VetoCap : option int
                }
  | VetoRejected

and fascist_response
    = Affiliations of affiliations

and mod_response
    = GameCap of game_cap_response

and game_cap_response
    = StartGameCap of int
    |   RemGameCap
