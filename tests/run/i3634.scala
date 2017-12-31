trait T {
  case object Foo
}

object Bar extends T

object Test {
  def main(args: Array[String]): Unit = {
    assert(Bar.Foo == Bar.Foo) // false
  }
}
