import scala.language.unsafeNulls

def test1 =
  val s: String = ???
  s match
    case _: String =>
    // under unsafeNulls, we should not get Match case Unreachable Warning
    case null => // ok

def test2 =
  val s: String | Null = ???
  s match
    case _: String =>
    case null =>
