case class Pair[A, B](_1: A, _2: B)

trait Cons[+H, +T]

object Cons {
  def apply[H, T](h: H, t: T): Cons[H, T] = ???
  def unapply[H, T](t: Cons[H, T]): Option[Pair[H, T]] = ???
}

object Test {
  def main(args: Array[String]): Unit = {
    Cons(Option(1), None) match {
      case Cons(Some(i), None) =>
        i: Int // error: found: Any(i), requires: Int
        assert(i == 1)
    }
  }
}
