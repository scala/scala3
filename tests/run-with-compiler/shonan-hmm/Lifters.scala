
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._
import scala.quoted.autolift._

object Lifters {
  implicit def LiftedClassTag[T: Type: ClassTag]  given QuoteContext: Expr[ClassTag[T]] = {
    '{ ClassTag(${the[ClassTag[T]].runtimeClass })}
  }

  implicit def ArrayIsLiftable[T : Type: ClassTag](implicit l: Liftable[T]): Liftable[Array[T]] = new Liftable[Array[T]] {
    def toExpr(x: Array[T]) given QuoteContext: Expr[Array[T]] = '{
      val array = new Array[T](${x.length})(${implicitly[Expr[ClassTag[T]]]})
      ${initArray(x, 'array)}
    }
  }

  implicit def IntArrayIsLiftable: Liftable[Array[Int]] = new Liftable[Array[Int]] {
    def toExpr(x: Array[Int]) given QuoteContext: Expr[Array[Int]] = '{
      val array = new Array[Int](${x.length})
      ${initArray(x, 'array)}
    }
  }

  private def initArray[T : Liftable : Type](arr: Array[T], array: Expr[Array[T]]) given QuoteContext: Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${i}) = ${x} }
      }.toList,
      array)
  }
}
