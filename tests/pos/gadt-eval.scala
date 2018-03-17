sealed trait Exp[T]
case class Lit(value: Int) extends Exp[Int]
case class Pair[A, B](fst: Exp[A], snd: Exp[B]) extends Exp[(A, B)]

object Test {
  def eval[T](e: Exp[T]): T = e match {
    case Lit(x) =>
      x
    case Pair(a, b) =>
      (eval(a), eval(b))
  }

  def eval2[T](e: Exp[T]): T = e match {
    case e: Lit =>
      e.value
    case e: Pair[t1, t2] =>
      (eval(e.fst), eval(e.snd))
  }
}
