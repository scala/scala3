//> using options -Werror

def boundary[T](body: (T => RuntimeException) => T): T =
  case class Break(value: T) extends RuntimeException
  try body(Break.apply)
  catch case Break(t) => t // error: pattern matching on local classes is unsound currently

def test =
  boundary[Int]: EInt =>
    val v: String = boundary[String]: EString =>
      throw EInt(3)
    v.length // a runtime error: java.lang.ClassCastException

def boundaryCorrectBehaviour[T](body: (T => RuntimeException) => T): T =
  object local:
    // A correct implementation, but is still treated as a local class right now
    case class Break(value: T) extends RuntimeException
  try body(local.Break.apply)
  catch case local.Break(t) => t // error
