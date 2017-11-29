open Types

fun main () =
    return <xml><body><table>
      <tr><td><a link={new_priv_game ()}>New Private Game</a></td></tr>
      <tr><td><a link={new_publ_game ()}>New Public Game</a></td></tr>
      <tr><td><a link={watch_game ()}>Watch Game</a></td></tr>
    </table></body></xml>

and new_priv_game () =
    r <- rand;
    return <xml>{[mod r 6]}</xml>

and new_publ_game () =
    return <xml>publ game</xml>

and watch_game () =
    return <xml>watch</xml>


fun game_loop (initial_state : game) =
    let fun loop_it ((me,chan,state) : client * channel Frontend.action * source game) : transaction {} =
            let fun loop () =
                    sleep 1;
                    loop ()
            in loop () end
    in me <- self;
       chan <- channel;
       (*dml (INSERT INTO users (Client, Chan, Game) VALUES ({[me]}, {[chan]}, {[initial_state.Game]}));*)

       state <- source initial_state;
       loop_it (me,chan,state) end
