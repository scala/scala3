// scalajs: --skip
// (use of reflection)

object Foo {
  object Bar
}

object Test:
  def main(args: Array[String]): Unit =
    for f <- Class.forName("Foo$").getDeclaredFields do
      println(f.getName)
