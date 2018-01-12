val sayings : list string = "" :: []

fun error_page (b : xbody) =
    return <xml><body>
      {b}
    </body></xml>
