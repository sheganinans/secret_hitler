val next_president_closure : int -> transaction ({} -> transaction int)

val eligible_chancelor : int * int -> int -> bool

datatype govt_vote_result = VoteSuccessful | VoteFailed | GovtInChaos

val vote_on_govt_closure : {} -> transaction (list bool -> transaction govt_vote_result)

type deck = list Types.side

type decks = { Draw : deck, Discard : deck }

val shuffle_deck : deck -> transaction deck

val investigate_loyalty : Types.game -> int -> Types.side

val call_special_election_closure : source int -> (int -> transaction {})

val policy_peek : {} -> {}

datatype execution_result = PlayerKilled | LiberalsWin

val execute_player : Types.game -> transaction execution_result

val veto_power : {} -> {}
