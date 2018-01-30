import scala.annotation.dependent

trait Foo {
  type Bar
}

trait Quux(val foo: Foo)

class Child(@dependent override val foo: Foo) extends Quux(foo)

object FooInt extends Foo {
  type Bar = Int
}

object Test {
  val quux = new Child(FooInt)
  val x: quux.foo.Bar = 3
}