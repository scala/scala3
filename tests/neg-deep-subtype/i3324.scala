//> using options -Xfatal-warnings

class Foo {
  def foo(x: Any): Boolean =
    x.isInstanceOf[List[String]]  // error
}
