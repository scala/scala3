object Foo {
  def f(z: => (Int, String)) = {
    val (x, y) = z
  }
}

object Test { def main(args: Array[String]): Unit = { Foo.f { (1, "a") } } }
