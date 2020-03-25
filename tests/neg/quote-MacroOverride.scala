object Test {

  abstract class A {
    inline def f1(): String = "A.f1"
    inline def f2(): String = "A.f2"
    def f3(): String = "A.f3"
  }

  object B extends A {
    override inline def f1(): String = "B.f1" // error
    override inline def f2(): String = "B.f2" // error
    override inline def f3(): String = "B.f3"
  }

}
