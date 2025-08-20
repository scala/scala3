import scala.language.experimental.subCases

object Test:
  val x: Option[Option[Int]] = ???
  x match
    case Some(x2) if true if x2 match
      case Some(x3) if false if x3 match
        case 1 => "a"
        case x if x % 2 == 0 if x match
          case 4 => "b"
          case 6 => "b"
    case None => "d"

  x match {
    case Some(x2) if x2 match {
      case Some (x3) if x3 match {
        case 1 => "a"
        case 2 => "b"
      }
    }
    case None => "d"
  }
