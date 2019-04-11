object Test {

  def main(args: Array[String]): Unit = {
    new Foo(foo)(foo)
    Foo(foo)(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

case class Foo(a: Int) erased (b: Int) {
  println("Foo")
}
