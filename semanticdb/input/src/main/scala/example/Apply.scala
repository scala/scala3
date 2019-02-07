package example

class TestApply {
  object Foo {
    def apply(x: Int) : Int = x
  }
  val z = Foo(1)
  val y = Foo.apply(1)
}