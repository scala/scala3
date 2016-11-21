
object Test {
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo

trait Foo {
  System.out.println(42)
}
