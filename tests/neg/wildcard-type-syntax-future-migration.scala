//> using options -Werror

import scala.language.`future-migration`

def test =
  Seq() match
    case _: List[_] => // error: migration warning
    case _: Seq[?] =>
