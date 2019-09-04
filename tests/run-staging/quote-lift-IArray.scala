import scala.quoted._
import scala.quoted.staging._
import given scala.quoted.autolift._

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    '{
      def p[T](arr: IArray[T]): Unit = {
        println(arr.asInstanceOf[Array[_]].mkString("[", ", ", "]"))
      }
      p(${IArray.empty[Boolean]})
      p(${IArray.empty[Byte]})
      p(${IArray.empty[Short]})
      p(${IArray.empty[Char]})
      p(${IArray.empty[Int]})
      p(${IArray.empty[Long]})
      p(${IArray.empty[Float]})
      p(${IArray.empty[Double]})
      p(${IArray.empty[String]})
      println()
      p(${IArray(true)})
      p(${IArray[Byte](1, 2)})
      p(${IArray[Short](2, 3)})
      p(${IArray[Char]('a', 'b')})
      p(${IArray[Int](4, 5)})
      p(${IArray[Long](6L, 7L)})
      p(${IArray[Float](2.1f, 3.2f)})
      p(${IArray[Double](2.2, 3.3)})
      p(${IArray("abc", "xyz")})
    }
  }
}