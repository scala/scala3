import scala.language.`3.4-migration`

def test =
  Seq() match
    case _: List[_] => // warn: migration warning
    case _: Seq[?] =>
