
object Test {
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo[Int]

trait Foo[T] {
  System.out.println(42)
}
