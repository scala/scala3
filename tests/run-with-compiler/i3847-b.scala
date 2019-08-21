import scala.quoted._
import scala.quoted.staging._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T], qctx: QuoteContext): Liftable[Array[List[T]]] = {
    new Liftable[Array[List[T]]] {
      def toExpr(arr: Array[List[T]]) = '{
        new Array[List[$t]](${arr.length.toExpr})
        // TODO add elements
      }
    }
  }
}

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    import Arrays._
    implicit val ct: Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Expr[Array[List[Int]]] = Array[List[Int]](List(1, 2, 3)).toExpr
    println(arr.show)
  }
}
