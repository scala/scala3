import scala.quoted._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T], ct: Expr[ClassTag[T]]): Liftable[Array[T]] = {
    new Liftable[Array[T]] {
      def toExpr(arr: Array[T]) given QuoteContext: Expr[Array[T]] = '{
        new Array[$t](${arr.length.toExpr})($ct)
        // TODO add elements
      }
    }
  }
}

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(this.getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    import Arrays._
    implicit val ct: Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Expr[Array[Int]] = Array[Int](1, 2, 3).toExpr
    println(arr.show)
  }
}
