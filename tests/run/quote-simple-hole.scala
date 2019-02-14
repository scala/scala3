object Test {
  def main(args: Array[String]): Unit = {
    val x = '{0}
    val y = '{$x}
    val z = '{${'{$y}}}
    assert(x eq y)
    assert(x eq z)

    val i = '[Int]
    val j = '[$i]
    assert(i eq j)
  }
}
