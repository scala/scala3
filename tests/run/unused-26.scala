object Test {
  def main(args: Array[String]): Unit = {
    col("abc")(true)
  }
  def col[S](s: String)(unused ev: Boolean): Unit = println(s)
}
