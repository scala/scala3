import scala.quoted.*
import scala.quoted.staging.*
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsToExpr[T: ToExpr](implicit t: Type[T], ct: Expr[ClassTag[T]]): ToExpr[Array[T]] = {
    new ToExpr[Array[T]] {
     def apply(arr: Array[T])(using Quotes) = '{
        new Array[t.Underlying](${Expr(arr.length)})($ct)
        // TODO add elements
      }
    }
  }
}

object Test {
  implicit val toolbox: scala.quoted.staging.Compiler = scala.quoted.staging.Compiler.make(this.getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    import Arrays.*
    implicit val ct: Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Expr[Array[Int]] = Expr(Array[Int](1, 2, 3))
    println(arr.show)
  }
}
