import scala.language.experimental.subCases

// single sub case can be on the same line as outer case
object Test:
  val x: Option[Option[Int]] = ???
  x match
    case Some(x2) if x2 match case Some(x3) => "aa"
    case Some(x2) if false if x2 match case Some(x3) if true => "aa"
    case Some(x2) if x2 match case Some(x3) if x2 match case Some(x3) => "bb"
    case Some(y2) if y2 match
      case Some(y3) if y3 match
        case 1 => "a"
        case 2 => "b"
    case Some(x2) if x2 match case Some(x3) if x3 match
      case 1 => "a"
      case 2 => "b"
    case None => "d"
