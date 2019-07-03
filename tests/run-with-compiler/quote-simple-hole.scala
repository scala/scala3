import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
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
