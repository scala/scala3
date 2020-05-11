import scala.quoted._
import scala.quoted.staging._
import scala.reflect.ClassTag

object Arrays {
  implicit def ArrayIsLiftable[T](using s: Scope)(using s.Type[T], s.Liftable[T]): s.Liftable[Array[List[T]]] = {
    new s.Liftable[Array[List[T]]] {
      def toExpr(arr: Array[List[T]]) = '{
        new Array[List[T]](${Expr(arr.length)})
        // TODO add elements
      }
    }
  }
}

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = usingNewScope { s ?=>
    import Arrays._
    implicit val ct: s.Expr[ClassTag[Int]] = '{ClassTag.Int}
    val arr: s.Expr[Array[List[Int]]] = ArrayIsLiftable[Int].toExpr(Array[List[Int]](List(1, 2, 3)))
    println(arr.show)
  }
}
