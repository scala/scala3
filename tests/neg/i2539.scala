object Foo {
  val f0: Function0[Int]{ def foo(): Int } = () => 42 // error

  val f1: Function1[Int, Int]{ def foo(): Int } = x1 => 42 // error

  val f26: Function26[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]{ def foo(): Int } =
    (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26) => 42 // error

  val if1: ImplicitFunction1[Int, Int]{ def foo(): Int } = implicit x1 => 42 // error

  abstract class Fun0[X] extends Function0[X]
  val fun0a: Fun0[Int] = () => 42
  val fun0b: Fun0[Int] { def foo(): Int } = () => 42 // error

  val pf: PartialFunction[Int, Int]{ def foo(): Int } = { case x1 => 42 } // error

}
