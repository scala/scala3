//> using options -experimental -language:experimental.erasedDefinitions

object Test {
  def main(args: Array[String]): Unit = {
    col("abc")(true)
  }
  def col[S](s: String)(erased ev: Boolean): Unit = println(s)
}
