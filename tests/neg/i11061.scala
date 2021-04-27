case class Foo(a: Int, b: Int)

object Test {
  def foo(x: Foo) = List(x).map(_ + _) // error

  def main(args: Array[String]): Unit = println(foo(Foo(3, 4)))
}
