trait A { self =>
  class Foo {
    inline def inlineMeth: Unit = {
      println(self)
    }
  }
}

case class A2() extends A {
  case class C() extends Foo with A

  val c = new C
  (new c.Foo).inlineMeth      // error
}

object Test {
  def main(args: Array[String]): Unit = {
    new A2
  }
}
