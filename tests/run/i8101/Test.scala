// scalajs: --skip

class Bar extends JavaFoo with Foo {
  def read(): Int = ???
}

@main def Test =
  val stdout = new Bar
