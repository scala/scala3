import scala.quoted._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T: Liftable](implicit t: Type[T], ct: Expr[ClassTag[T]]): Liftable[Array[T]] = {
    new Liftable[Array[T]] {
      def toExpr(arr: Array[T])(implicit st: StagingContext): Expr[Array[T]] = '{
        new Array[$t](${arr.length.toExpr})($ct)
        // TODO add elements
      }
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(this.getClass.getClassLoader)
    import Arrays._
    implicit def ct: Staged[ClassTag[Int]] = '{ClassTag.Int}
    def arr: Staged[Array[Int]] = Array[Int](1, 2, 3).toExpr
    val tb = Toolbox.make
    println(tb.show(arr))
  }
}