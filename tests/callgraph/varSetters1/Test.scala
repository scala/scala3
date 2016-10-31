
object Test {
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    System.out.println(foo.bar)
    foo.baz()
    System.out.println(foo.bar)
  }
}


class Foo {
  var bar = 1
  def baz(): Unit = {
    bar = 42
  }
}