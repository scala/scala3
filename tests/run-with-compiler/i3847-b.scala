import scala.quoted._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T]): Liftable[Array[List[T]]] = {
    new Liftable[Array[List[T]]] {
      def toExpr(arr: Array[List[T]])(implicit st: StagingContext): Expr[Array[List[T]]] = '{
        new Array[List[$t]](${arr.length.toExpr})
        // TODO add elements
      }
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    import Arrays._
    implicit val ct: Staged[ClassTag[Int]] = '{ClassTag.Int}
    val arr: Staged[Array[List[Int]]] = Array[List[Int]](List(1, 2, 3)).toExpr
    val tb = Toolbox.make
    println(tb.show(arr))
  }
}
