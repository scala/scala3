//> using options -Werror

import scala.language.`3.3`

def test =
  Seq() match
    case _: List[_] =>
    case _: Seq[?] =>
