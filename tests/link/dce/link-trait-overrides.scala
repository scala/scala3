
object Test {
  def main(args: Array[String]): Unit = {
    new Foo().foo
  }
}

class Foo extends Bar

trait Bar extends Baz {
  override def foo = System.out.println(42)
}

trait Baz {
  def foo = System.out.println(5)
}
