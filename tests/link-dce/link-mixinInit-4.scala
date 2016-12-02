
object Test {
  def main(args: Array[String]): Unit = {
    new Bar
  }
}

class Bar extends Foo[Int](42)

trait Foo[T](n: Int) {
  System.out.println(n)
}
