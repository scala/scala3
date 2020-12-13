
object Baz {
  class Foo {
    private var v = 0
    inline def run1 = {
      v += { v = 1 }  // error
      v
    }
    inline def run2: Int = {
      v += { v = 1 }  // error
      v
    }
  }
  val foo = new Foo
  foo.run1
  foo.run2
}