import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    '{
      def p[T](arr: IArray[T]): Unit = {
        println(arr.asInstanceOf[Array[_]].mkString("[", ", ", "]"))
      }
      p(${Expr(IArray.empty[Boolean])})
      p(${Expr(IArray.empty[Byte])})
      p(${Expr(IArray.empty[Short])})
      p(${Expr(IArray.empty[Char])})
      p(${Expr(IArray.empty[Int])})
      p(${Expr(IArray.empty[Long])})
      p(${Expr(IArray.empty[Float])})
      p(${Expr(IArray.empty[Double])})
      p(${Expr(IArray.empty[String])})
      println()
      p(${Expr(IArray(true))})
      p(${Expr(IArray[Byte](1, 2))})
      p(${Expr(IArray[Short](2, 3))})
      p(${Expr(IArray[Char]('a', 'b'))})
      p(${Expr(IArray[Int](4, 5))})
      p(${Expr(IArray[Long](6L, 7L))})
      p(${Expr(IArray[Float](2.1f, 3.2f))})
      p(${Expr(IArray[Double](2.2, 3.3))})
      p(${Expr(IArray("abc", "xyz"))})
    }
  }
}