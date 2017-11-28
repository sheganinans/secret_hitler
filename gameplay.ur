open Types

fun next_president_closure (max : int) : transaction ({} -> transaction int) =
    let fun next (src : source int) : {} -> transaction int =
         fn {} =>
            curr <- get src;
            set src (if curr = max
                     then        1
                     else curr + 1);
            get src
    in src <- source 0;
       return (next src) end

fun eligible_chancelor ((prev_pres, prev_chan) : int * int) (poss_chan : int) : bool =
    poss_chan <> prev_pres &&
    poss_chan <> prev_chan

datatype govt_vote_result = VoteSuccessful | VoteFailed | GovtInChaos

fun vote_on_govt_closure () : transaction (list bool -> transaction govt_vote_result) =
    let fun vote (rej_counter : source int) : list bool -> transaction govt_vote_result =
         fn votes =>
            let fun filter_count (b : bool) : int =
                    List.filter (fn v => v = b) votes |> List.length
            in rejs <- get rej_counter;
               if filter_count True > filter_count False
               then      set rej_counter          0; return VoteSuccessful
               else if rejs = 2
                    then set rej_counter          0; return GovtInChaos
                    else set rej_counter (rejs + 1); return VoteFailed
            end
    in rejection_counter <- source 0;
       return (vote rejection_counter)
    end

type deck = list side

type decks = { Draw : deck, Discard : deck }

fun shuffle_deck (deck : deck) : transaction deck =
    (* Could be optimized, replace inner List.length calls with integer acc. *)
    let fun go (shuffled, draw) : transaction deck =
            if List.length draw = 1
            then return (List.append draw shuffled)
            else
                let fun take_item_at (i : int) (deck : deck) : side * deck =
                        let fun loop ((left, right) : deck * deck) : side * deck =
                                case right of
                                | (card :: rest) =>
                                  if List.length left = i (* Replace *)
                                  then (card, List.append left right)
                                  else loop (card :: left, rest)
                                | _ => error <xml>Shuffle: This should never happen.</xml>
                        in loop ([], deck) end
                in r <- rand;
                   let val (card, rest) = take_item_at (mod r (List.length deck)) deck (* Replace *)
                   in go (card :: shuffled, rest) end
                end
    in go ([], deck) end

fun investigate_loyalty (game : game) (id : int) : side =
    if List.exists (fn player => player.Id = id) game.Liberals
    then Liberal
    else Fascist

fun call_special_election_closure (president : source int) : int -> transaction {} =
    (* TODO: Implement case where special election is called for the next player in rotation.
             In this case, this player gets to run for president twice. *)
    fn i => set president i

fun policy_peek () =
    (* TODO: Requires frontend to implement this. *)
    ()

datatype execution_result = PlayerKilled | LiberalsWin

fun execute_player (game : game) : transaction execution_result = return PlayerKilled

fun veto_power () = ()
