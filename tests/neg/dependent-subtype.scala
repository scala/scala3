import scala.annotation.dependent

trait Foo {
  type Bar
}

class Quux(@dependent val foo: Foo)
class Child1(@dependent override val foo: Foo) extends Quux(foo)
class Child2(override val foo: Foo) extends Quux(foo)
class Child3(foo: Foo) extends Quux(foo)

object FooInt extends Foo {
  type Bar = Int
}

object Test {
  val quux1 = new Child1(FooInt)
  val x1: quux1.foo.Bar = 3          // ok

  val quux2 = new Child2(FooInt)
  val x2: quux2.foo.Bar = 3         // error

  val quux3 = new Child3(FooInt)
  val x3: quux3.foo.Bar = 3         // error
}


object Test2 {
  trait C
  class D extends C
  class E extends D

  class Bar(@dependent o: C)

  val x1 = new Bar(new D)
  val d: D = x1.o           // ok

  val x2 = new Bar(new E)
  val e: E = x2.o          // ok

  def f(x: Int): E = ???

  val x3 = new Bar(f(5))
  val e2: E = x3.o       // ok
}