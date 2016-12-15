
object Test {
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo(42)

trait Foo(n: Int) {
  System.out.println(n)
}
