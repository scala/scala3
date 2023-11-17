import scala.language.`future-migration`

def test =
  Seq() match
    case _: List[_] => // warn: migration warning
    case _: Seq[?] =>
