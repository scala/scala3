import scala.language.experimental.subCases

// single sub case can be one the same line as outer case
object Test:
  val x: Option[Option[Int]] = ???
  x match
    case Some(x2) with x2 match case Some(x3) => "aa"
    case Some(x2) if false with x2 match case Some(x3) if true => "aa"
    case Some(x2) with x2 match case Some(x3) with x2 match case Some(x3) => "bb"
    case Some(y2) with y2 match
      case Some(y3) with y3 match
        case 1 => "a"
        case 2 => "b"
    case Some(x2) with x2 match case Some(x3) with x3 match
      case 1 => "a"
      case 2 => "b"
    case None => "d"
