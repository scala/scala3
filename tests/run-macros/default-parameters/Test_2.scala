object Test {
  def main(args : Array[String]) : Unit =
    printDefaultParameters {
      class A(a: Int = 1):
        def f(b1: Int = 1, b2: Int = 2): Int = ???
        def g(c1: Int = 1)(c2: Int = 2): Int = ???
        def f2(d0: Int, d1: Int = 1, d2: Int = 2): Int = ???
      new B(a = 4)
      B.f(a = 4)
    }
}

class B(a: Int = 1)
object B:
  def f(a: Int = 1) = 9

