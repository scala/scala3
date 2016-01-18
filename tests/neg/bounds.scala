object Test {
  class C[B >: String <: Int](x: B)
  def g[B >: String <: Int](x: B): Int = x
  def main(args: Array[String]): Unit = {
    g("foo")
    new C("bar")
  }
  def baz[X >: Y, Y <: String](x: X, y: Y) = (x, y)

  baz[Int, String](1, "abc")

}
