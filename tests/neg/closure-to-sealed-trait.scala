sealed trait Foo {
  def foo(x: String): String
}
def test: Foo = { (x: String) => "" } // error
