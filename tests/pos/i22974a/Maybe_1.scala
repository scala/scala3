package pack
import Maybe._
opaque type Maybe[+A] >: (Absent | Present[A]) = Absent | Present[A]
object Maybe:
  sealed abstract class Absent
  case object Absent extends Absent
  object internal:
    case class PresentAbsent(val depth: Int)
  opaque type Present[+A] = A | internal.PresentAbsent

  extension [A](self: Maybe[A]) {
    inline def flatten[B]: Maybe[B] = ???
    inline def isDefined: Boolean = ???
  }
