
object Test {
  case class Foo(x: Int)

  def main(args: Array[String]): Unit = {
    Foo(42) match {
      case Foo(x) => System.out.println(x)
      case _ =>
    }
  }
}
