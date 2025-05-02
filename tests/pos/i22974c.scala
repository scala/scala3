object other:
  sealed abstract class Absent
  case object Absent extends Absent
  case class PresentAbsent(val depth: Int)
  opaque type Present[+A] = A | PresentAbsent
  opaque type Maybe[+A] >: (Absent | Present[A]) = Absent | Present[A]

  extension [A](self: Maybe[A]) {
    inline def flatten[B]: Maybe[B] = if self.isEmpty then Absent else ???
    def isEmpty: Boolean = self.isInstanceOf[Absent]
  }

class Test {
  def main(): Unit =
    import other.Maybe
    val res: Maybe[Maybe[Int]] = ???
    res.flatten
}
