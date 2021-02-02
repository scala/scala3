object Test {

  abstract class A {
    def f1(): Int = 1
    def f2(): Int
    inline def f3(): Int
    inline def f4(): Int = 1
  }

  class B extends A {
    override def f1(): Int = 1 // OK
    override def f2(): Int = 1 // OK
    override def f3(): Int = 1 // error
    override def f4(): Int = 1 // error
  }

  class C extends A {
    inline override def f1(): Int = 1 // OK
    inline override def f2(): Int = 1 // OK retained
    inline override def f3(): Int = 1 // OK not retianed
    inline override def f4(): Int = 1 // error
  }

  abstract class D extends A {
    override def f1(): Int // OK
    override def f2(): Int // OK
    override def f3(): Int // error
    override def f4(): Int // error
  }

  abstract class E extends A { // error f1
    inline override def f1(): Int
    inline override def f2(): Int
    inline override def f3(): Int
    inline override def f4(): Int // error
  }

}
