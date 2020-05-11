import scala.quoted._
import scala.quoted.staging._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T](using s: Scope)(implicit t: s.Type[T], ct: s.Expr[ClassTag[T]], lf: s.Liftable[T]): s.Liftable[Array[T]] = {
    new s.Liftable[Array[T]] {
      def toExpr(arr: Array[T]) = '{
        new Array[$t](${Expr(arr.length)})($ct)
        // TODO add elements
      }
    }
  }
}

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(this.getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope { s ?=>
    import Arrays._
    given s.Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: s.Expr[Array[Int]] = ArrayIsLiftable[Int].toExpr(Array[Int](1, 2, 3))
    println(arr.show)
  }
}
