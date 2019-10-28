

object Foo {
  def eq(x: Foo, y: Foo): Boolean = Bar.eqMacro(x, y)
  def plus(x: Foo, y: Foo): Foo = Bar.plusMacro(x, y)
}

class Foo(val value: Int)
