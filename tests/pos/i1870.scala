trait Stream[+A]
case class Unfold[S,+A](s: S, f: S => Option[(A,S)]) extends Stream[A]

object Test {
  def unbox[A](s: Stream[A]) = s match {
    case Unfold(s, f) =>
      val s1 = s
      val f1 = f
      (s, f)
  }
}
