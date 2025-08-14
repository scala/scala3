import scala.language.experimental.matchWithSubCases

object Test:
  val x: Option[Option[Int]] = ???
  x match
    case Some(x2) with x2 match
      case Some(x3) with x3 match
        case 1 => "a"
        case 2 => "b"
    case None => "d"

  x match {
    case Some(x2) with x2 match {
      case Some (x3) with x3 match {
        case 1 => "a"
        case 2 => "b"
      }
    }
    case None => "d"
  }
