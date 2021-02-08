trait Foo {
  object bar {
    private def fn() = 5
  }
}

trait Foo2 {
  object bip {
    def fn() = 10
  }
}

class Bob extends AnyRef with Foo with Foo2 {
  import bip.*
  import bar.*

  def go() = fn()
}
