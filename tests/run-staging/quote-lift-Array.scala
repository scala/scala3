import scala.quoted.*
import scala.quoted.staging.*
object Test {
  implicit val toolbox: scala.quoted.staging.Compiler = scala.quoted.staging.Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    '{
      def p[T](arr: Array[T]): Unit = {
        println(arr.asInstanceOf[Array[_]].mkString("[", ", ", "]"))
      }
      p(${Expr(Array.empty[Boolean])})
      p(${Expr(Array.empty[Byte])})
      p(${Expr(Array.empty[Short])})
      p(${Expr(Array.empty[Char])})
      p(${Expr(Array.empty[Int])})
      p(${Expr(Array.empty[Long])})
      p(${Expr(Array.empty[Float])})
      p(${Expr(Array.empty[Double])})
      p(${Expr(Array.empty[String])})
      println()
      p(${Expr(Array(true))})
      p(${Expr(Array[Byte](1, 2))})
      p(${Expr(Array[Short](2, 3))})
      p(${Expr(Array[Char]('a', 'b'))})
      p(${Expr(Array[Int](4, 5))})
      p(${Expr(Array[Long](6L, 7L))})
      p(${Expr(Array[Float](2.1f, 3.2f))})
      p(${Expr(Array[Double](2.2, 3.3))})
      p(${Expr(Array("abc", "xyz"))})
    }
  }
}