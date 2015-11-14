object Test {
  class C[B >: String <: Int](x: B)
  def g[B >: String <: Int](x: B): Int = x
  def main(args: Array[String]): Unit = {
    g("foo")
    new C("bar")
  }
}
