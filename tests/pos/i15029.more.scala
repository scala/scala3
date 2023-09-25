//> using options -Werror

// Like tests/pos/i15029.scala,
// but with a more complicated prefix
// and Schema[String]

sealed trait Schema[A]

sealed class Universe:
  sealed trait Instances[B]:
    case class Field() extends Schema[B]
    case object Thing extends Schema[B]

object Universe1 extends Universe
object Universe2 extends Universe

object Ints extends Universe1.Instances[Int]
object Strs extends Universe2.Instances[String]

// Match not exhaustive error! (with fatal warnings :P)
class Test:
  def handle(schema: Schema[String]) =
    schema match // was: match may not be exhaustive
      case Strs.Field() =>
      case Strs.Thing   =>
