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
