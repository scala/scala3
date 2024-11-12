//> using options  -Wunused:implicits

/* This goes around the "trivial method" detection */
val default_int = 1

object Xd {
  private def f1(a: Int) = a // OK
  private def f2(a: Int) = 1 // OK
  private def f3(a: Int)(using Int) = a // warn
  private def f4(a: Int)(using Int) = default_int // warn
  private def f6(a: Int)(using Int) = summon[Int] // OK
  private def f7(a: Int)(using Int) = summon[Int] + a // OK
  private def f8(a: Int)(using foo: Int) = a // warn
  private def f9(a: Int)(using Int) = ??? // OK trivial
  private def g1(a: Int)(implicit foo: Int) = a // warn
}
