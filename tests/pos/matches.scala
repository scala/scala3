package matches
object Test:
  2 min 3 match
    case 2 => "OK"
    case 3 => "?"
  match
    case "OK" =>
    case "?" => throw new AssertionError()

  val x = 4
  if 2 < 3
     || x.match
          case 4 => true
          case _ => false
     || (2 + 2).match
          case 4 => true
          case _ => false
  then
    println("ok")
end Test
