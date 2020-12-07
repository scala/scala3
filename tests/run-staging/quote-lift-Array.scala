import scala.quoted._
import scala.quoted.staging._
object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    '{
      def p[T](arr: Array[T]): Unit = {
        println(arr.asInstanceOf[Array[_]].mkString("[", ", ", "]"))
      }
      p(${Value(Array.empty[Boolean])})
      p(${Value(Array.empty[Byte])})
      p(${Value(Array.empty[Short])})
      p(${Value(Array.empty[Char])})
      p(${Value(Array.empty[Int])})
      p(${Value(Array.empty[Long])})
      p(${Value(Array.empty[Float])})
      p(${Value(Array.empty[Double])})
      p(${Value(Array.empty[String])})
      println()
      p(${Value(Array(true))})
      p(${Value(Array[Byte](1, 2))})
      p(${Value(Array[Short](2, 3))})
      p(${Value(Array[Char]('a', 'b'))})
      p(${Value(Array[Int](4, 5))})
      p(${Value(Array[Long](6L, 7L))})
      p(${Value(Array[Float](2.1f, 3.2f))})
      p(${Value(Array[Double](2.2, 3.3))})
      p(${Value(Array("abc", "xyz"))})
    }
  }
}