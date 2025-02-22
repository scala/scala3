//> using options  -Wunused:params

/* This goes around the "trivial method" detection */
object Foo {
  val default_int = 1

  private def f1(a: Int) = a // OK
  private def f2(a: Int) = default_int // warn
  private def f3(a: Int)(using Int) = a // warn
  private def f4(a: Int)(using Int) = default_int // warn // warn
  private def f6(a: Int)(using Int) = summon[Int] // warn
  private def f7(a: Int)(using Int) = summon[Int] + a // OK
  /* --- Trivial method check --- */
  private def g1(x: Int) = 1 // OK
  private def g2(x: Int) = ??? // OK
}

package foo.test.i17101:
  type Test[A] = A
  extension[A] (x: Test[A]) { // OK
    def value: A = x
    def causesIssue: Unit = println("oh no")
    def isAnIssue(y: A): Boolean = x == x // warn
  }
