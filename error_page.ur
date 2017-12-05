val sayings : list string = "" :: []

fun error_page (b : xbody) =
    return <xml><body>
      ERROR 500!! To be beautified..<br/>
      {b}
    </body></xml>
