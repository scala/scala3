class A { self =>
  class Foo {
    transparent def inlineMeth: Unit = {
      println(self)
    }
  }
}

class C extends A {
  class B extends A
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    val b = new c.B

    (new b.Foo).inlineMeth
  }
}
