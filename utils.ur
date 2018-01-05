fun mapM_ [a] [b] (f : a -> transaction b) (x : list a) : transaction {} =
    _ <- List.mapM f x; return {}

fun fold_css (l : list css_class) : css_class = List.foldr (fn c s => classes c s) null l

fun shuffle [a] (deck : list a) : transaction (list a) =
    let fun take_item_at [a] (i : int) (deck : list a) : a * list a =
            let fun loop [a] ((left, right, len) : list a * list a * int) : a * list a =
                    case right of
                    | (card :: rest) =>
                      if len = i
                      then (card, List.append left right)
                      else loop (card :: left, rest, len + 1)
                    | _ => error <xml>Shuffle: This should never happen.</xml>
            in loop ([], deck, 0) end

        fun go (shuffled, draw, len) : transaction (list a) =
            case len of
            | 1 => return (List.append draw shuffled)
            | _ => r <- rand;
                   let val (card, rest) = take_item_at (mod r len) deck
                   in go (card :: shuffled, rest, len - 1) end
    in go ([], deck, List.length deck) end


fun generate_top_3_cards (libs : int) (fasc : int) : transaction (bool * bool * bool) =
    r1 <- rand;
    r2 <- rand;
    r3 <- rand;
    let fun delta_deck (b : bool) (libs : int) (fasc : int) : int * int =
            if b
            then (libs - 1, fasc    )
            else (libs    , fasc - 1)
        val fst = Basis.mod r1 (libs + fasc) < libs
        val (libs, fasc) = delta_deck fst libs fasc
        val snd = Basis.mod r2 (libs + fasc) < libs
        val (libs, fasc) = delta_deck snd libs fasc
        val trd = Basis.mod r3 (libs + fasc) < libs
    in  return (fst, snd, trd)
    end

fun new_lib_fasc_ratio (f : int -> int -> int)
                       ((c1, c2, c3) : bool * bool * bool)
                       (lib : int)
                       (fasc : int) : int * int =
    let fun delta_ratio (lib : int) (fasc : int) (b : bool) : int * int =
            if b then (f lib 1, fasc) else (lib, f fasc 1)
        val (lib, fasc) = delta_ratio lib fasc c1
        val (lib, fasc) = delta_ratio lib fasc c2
    in  delta_ratio lib fasc c3
    end

fun new_lib_fasc_draw_ratio (top_3 : bool * bool * bool) (lib : int) (fasc : int) : int * int =
    new_lib_fasc_ratio plus  top_3                        lib         fasc

fun new_lib_fasc_disc_ratio (top_3 : bool * bool * bool) (lib : int) (fasc : int) : int * int =
    new_lib_fasc_ratio minus top_3                        lib         fasc

fun next_turn_deck_state (current_turn : { LiberalsInDraw : int, FascistsInDraw : int
                                         , LiberalsInDisc : int, FascistsInDisc : int })
    : transaction { Fst : bool, Snd : bool, Trd : bool
                  , LibDraw : int, FasDraw : int
                  , LibDisc : int, FasDisc : int } =
    if current_turn.LiberalsInDraw + current_turn.FascistsInDraw < 3
    then ((fst, snd, trd) <- generate_top_3_cards
                                 (current_turn.LiberalsInDraw + current_turn.LiberalsInDisc)
                                 (current_turn.FascistsInDraw + current_turn.FascistsInDisc);
          let val (lib_draw, fasc_draw) = new_lib_fasc_draw_ratio
                                              (fst, snd, trd)
                                              current_turn.LiberalsInDraw
                                              current_turn.FascistsInDraw
          in  return { Fst = fst, Snd = snd, Trd = trd
                     , LibDraw = lib_draw, FasDraw = fasc_draw
                     , LibDisc =        0, FasDisc =         0 }
          end)
    else ((fst, snd, trd) <- generate_top_3_cards current_turn.LiberalsInDraw
                                                  current_turn.FascistsInDraw;
          let val (lib_draw, fasc_draw) = new_lib_fasc_draw_ratio
                                              (fst, snd, trd)
                                              current_turn.LiberalsInDraw
                                              current_turn.FascistsInDraw
              val (lib_disc, fasc_disc) = new_lib_fasc_disc_ratio
                                              (fst, snd, trd)
                                              current_turn.LiberalsInDisc
                                              current_turn.FascistsInDisc
          in return { Fst = fst, Snd = snd, Trd = trd
                    , LibDraw = lib_draw, FasDraw = fasc_draw
                    , LibDisc = lib_disc, FasDisc = fasc_disc }
          end)
