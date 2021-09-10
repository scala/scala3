// Like the "tricksier" variantion in t6146, but less tricksy (less nesting)
// but also with a non-Any type argument, and some invariant usage of the type.
// Used to study why `case _: C.type =>` is considered unreachable
// in the ordinal method object A synthesises.
trait T[X] {
  sealed trait A
  object A {
    case object B extends A
  }

  def give: X
  def take(x: X): X
}

object O extends T[String] {
  def give            = "O"
  def take(x: String) = s"$x. Love, O."
}

case object C extends O.A
