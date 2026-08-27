//> using options -Werror

object O:
  opaque type T = String

  transparent inline def make: T =
    println("effect")
    "value"

// shouldn't emit E129: Pure Expression In Statement Position
object Statement:
  O.make
