

class Foo {
  def foo(x: Any): Boolean =
    x.isInstanceOf[List[String]]  // warn
}
