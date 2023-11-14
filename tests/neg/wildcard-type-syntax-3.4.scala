//> using options -Werror

import scala.language.`3.4-migration`

def test =
  Seq() match
    case _: List[_] => // error: migration warning
    case _: Seq[?] =>
