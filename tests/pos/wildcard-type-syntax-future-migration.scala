import scala.language.`future-migration`

def test =
  Seq() match
    case _: List[_] => // warn
    case _: Seq[?] =>
