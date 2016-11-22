
object Test {
  def main(args: Array[String]): Unit = {
    foo(new Foo)
  }

  def foo(s: Object) = {
    System.out.println(s)
  }

}

class Foo {
  override def toString: String = "Foo.toString"
}
