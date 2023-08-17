//> using options -Werror
sealed trait Schema[A]

sealed trait RecordInstances:
  case class Field[B]() extends Schema[B]
  case object Thing extends Schema[Int]

object X extends RecordInstances
object Y extends RecordInstances

// Match not exhaustive error! (with fatal warnings :P)
class Test:
  def handle[T](schema: Schema[T]) =
    schema match // was: match may not be exhaustive
      case X.Field() =>
      case X.Thing   =>
      case Y.Field() =>
      case Y.Thing   =>
