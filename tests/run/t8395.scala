 object Test {
  def baz(x: Object) = {
    val (_s: String) as s = x
    x
  }
  def main(args: Array[String]): Unit = {
    assert(baz("1") == "1")
  }
}
