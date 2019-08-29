import scala.quoted._
import scala.quoted.staging._
import given scala.quoted.autolift._

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    '{
      def p[T](arr: Array[T]): Unit = {
        println(arr.asInstanceOf[Array[_]].mkString("[", ", ", "]"))
      }
      p(${Array.empty[Boolean]})
      p(${Array.empty[Byte]})
      p(${Array.empty[Short]})
      p(${Array.empty[Char]})
      p(${Array.empty[Int]})
      p(${Array.empty[Long]})
      p(${Array.empty[Float]})
      p(${Array.empty[Double]})
      p(${Array.empty[String]})
      println()
      p(${Array(true)})
      p(${Array[Byte](1, 2)})
      p(${Array[Short](2, 3)})
      p(${Array[Char]('a', 'b')})
      p(${Array[Int](4, 5)})
      p(${Array[Long](6L, 7L)})
      p(${Array[Float](2.1f, 3.2f)})
      p(${Array[Double](2.2, 3.3)})
      p(${Array("abc", "xyz")})
    }
  }
}