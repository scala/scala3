//> using options -explain

object Matches:
  def unapply(y: Any)[T]: Option[Any] = None

def main =
  42 match
    case Matches(x) => println(x) // error // error
