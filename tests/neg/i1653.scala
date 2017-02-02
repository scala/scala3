trait Foo {
  def foo() = new Unit with Foo  // error: cannot extend final class Unit  // error: illegal trait inheritance
}
