object Test {
  def main(args: Array[String]): Unit = {
    col("abc")(true)
  }
  def col[S](s: String)(ghost ev: Boolean): Unit = println(s)
}
