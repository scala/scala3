import scala.language.experimental.modularity

class UnspecificBox(val v: Any)

def test =
  val v1: UnspecificBox(4) = UnspecificBox(4) // warn
