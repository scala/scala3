//> using options -Werror
sealed trait Schema[A]

object Schema extends RecordInstances

sealed trait RecordInstances:
  case class Field[A]() extends Schema[A]
  case object Thing extends Schema[Int]

import Schema._

// Match not exhaustive error! (with fatal warnings :P)
def handle[A](schema: Schema[A]) =
  schema match
    case Field() => println("field")
    case Thing   => println("thing")
