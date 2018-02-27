sealed trait Exp[T]
case class Lit(value: Int) extends Exp[Int]
case class Pair[A, B](fst: Exp[A], snd: Exp[B]) extends Exp[(A, B)]

object Test {
  def eval[T](e: Exp[T]): T = e match {
    case Lit(x) =>
      x
    case Pair(a, b) =>
      (eval(a), eval(a)) // error:
        // -- [E007] Type Mismatch Error: tests/neg/gadt-eval.scala:10:6 ------------------
        // 10 |      (eval(a), eval(a))
        // |      ^^^^^^^^^^^^^^^^^^
        // |    found:    (_$1, _$1)
        // |    required: T
        // |
        // |    where:    T is a type in method eval which is an alias of (_$1, _$2)
  }

  def eval2[T](e: Exp[T]): T = e match {
    case e: Lit =>
      e.value
    case e: Pair[type t1, type t2] =>
      (eval(e.fst), eval(e.fst)) // error:
        //-- [E007] Type Mismatch Error: tests/neg/gadt-eval.scala:24:6 ------------------
        //24 |      (eval(e.fst), eval(e.fst))
        //   |      ^^^^^^^^^^^^^^^^^^^^^^^^^^
        //   |     found:    (t1, t1)
        //   |     required: T
        //   |
        //   |     where:    T is a type in method eval2 which is an alias of (t1, t2)
  }
}
