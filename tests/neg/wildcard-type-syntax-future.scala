import scala.language.future

def test =
  Seq() match
    case _: List[_] => // error
    case _: Seq[?] =>
