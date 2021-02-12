import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuotes {
    val x = '{0}
    val y = '{$x}
    val z = '{${'{$y}}}
    val a = '{{{$x}}}
    val b = '{{${{'{{$y}}}}}}
    assert(x eq y)
    assert(x eq z)
    assert(x eq a)
    assert(x eq b)

    val i = Type.of[Int]
    val j = Type.of[i.Underlying]
    assert(i eq j)
  }
}
