import scala.quoted.*
import scala.quoted.staging.*
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsToExpr[T: ToExpr](implicit t: Type[T], qctx: Quotes): ToExpr[Array[List[T]]] = {
    new ToExpr[Array[List[T]]] {
      def apply(arr: Array[List[T]])(using Quotes) = '{
        new Array[List[T]](${Expr(arr.length)})
        // TODO add elements
      }
    }
  }
}

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    import Arrays.*
    implicit val ct: Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Expr[Array[List[Int]]] = Expr(Array[List[Int]](List(1, 2, 3)))
    println(arr.show)
  }
}
