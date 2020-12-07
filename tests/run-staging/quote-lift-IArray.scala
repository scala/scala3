import scala.quoted._
import scala.quoted.staging._

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    '{
      def p[T](arr: IArray[T]): Unit = {
        println(arr.asInstanceOf[Array[_]].mkString("[", ", ", "]"))
      }
      p(${Value(IArray.empty[Boolean])})
      p(${Value(IArray.empty[Byte])})
      p(${Value(IArray.empty[Short])})
      p(${Value(IArray.empty[Char])})
      p(${Value(IArray.empty[Int])})
      p(${Value(IArray.empty[Long])})
      p(${Value(IArray.empty[Float])})
      p(${Value(IArray.empty[Double])})
      p(${Value(IArray.empty[String])})
      println()
      p(${Value(IArray(true))})
      p(${Value(IArray[Byte](1, 2))})
      p(${Value(IArray[Short](2, 3))})
      p(${Value(IArray[Char]('a', 'b'))})
      p(${Value(IArray[Int](4, 5))})
      p(${Value(IArray[Long](6L, 7L))})
      p(${Value(IArray[Float](2.1f, 3.2f))})
      p(${Value(IArray[Double](2.2, 3.3))})
      p(${Value(IArray("abc", "xyz"))})
    }
  }
}