object Test {
  def g[B >: String <: Int](x: B): Int = x
  def main(args: Array[String]): Unit = {
    g("foo") // error: Type argument String' does not conform to upper bound Int
  }
  def baz[X >: Y, Y <: String](x: X, y: Y) = (x, y)

  baz[Int, String](1, "abc") // error: Type argument Int does not conform to lower bound Y

}
