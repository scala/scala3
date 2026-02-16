import language.experimental.namedTuples

def find(explore: List[(seen: Set[Int], x: Int, y: Int)]): Any =
  explore match
    case Nil => ???
    case (seen = s, x = x, y = y) :: rest => ???