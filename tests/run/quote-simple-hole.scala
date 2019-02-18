object Test {
  def main(args: Array[String]): Unit = {
    val x = '{0}
    val y = '{$x}
    val z = '{${'{$y}}}
    val a = '{{{$x}}}
    val b = '{{${{'{{$y}}}}}}
    assert(x eq y)
    assert(x eq z)
    assert(x eq a)
    assert(x eq b)

    val i = '[Int]
    val j = '[$i]
    assert(i eq j)
  }
}
