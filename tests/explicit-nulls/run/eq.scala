
object Test {

  def main(args: Array[String]): Unit = {
    val x: String|Null = ""
    val y: String|Null = null
    val z: String = ""

    assert(x == x)
    assert(x == "")
    assert("" == x)
    assert(x == z)
    assert(z == x)
    assert(x != "xx")
    assert(x != y)
    assert(y == y)
    assert(z.asInstanceOf[String|Null] != null)
    assert(z.asInstanceOf[Any] != null)

    assert(x != null)
    assert(null != x)
    assert(y == null)
    assert(null == y)
    assert(null == null)
  }
}
