trait A { def i = 1 }
class B extends A {
  class C {
    val x = B.super.i  // needs accessor
  }
  val f: Int => Int =
    _ => super.i // OK
  trait Sammy {
    def run(x: Int): Int
  }
  val s1: Sammy = (x: Int) => super.i // OK
  abstract class Sam {
    def run(x: Int): Int
  }
  val s2: Sam = (x: Int) => super.i // needs accessor
  val p: PartialFunction[Int, Int] = {
    case _ => super.i // needs accessor
  }
}