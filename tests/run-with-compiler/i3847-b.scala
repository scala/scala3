import scala.quoted._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T]): Liftable[Array[List[T]]] = {
    new Liftable[Array[List[T]]] {
      def toExpr(arr: Array[List[T]]) given QuoteContext: Expr[Array[List[T]]] = '{
        new Array[List[$t]](${arr.length.toExpr})
        // TODO add elements
      }
    }
  }
}

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    import Arrays._
    implicit val ct: Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Expr[Array[List[Int]]] = Array[List[Int]](List(1, 2, 3)).toExpr
    println(arr.show)
  }
}