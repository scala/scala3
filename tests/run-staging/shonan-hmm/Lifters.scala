
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._
import scala.quoted.autolift.given

object Lifters {
  implicit def LiftedClassTag[T: TypeTag: ClassTag] (given QuoteContext): Expr[ClassTag[T]] = {
    '{ ClassTag(${summon[ClassTag[T]].runtimeClass })}
  }

  implicit def ArrayIsLiftable[T : TypeTag: ClassTag](implicit l: Liftable[T]): Liftable[Array[T]] = new Liftable[Array[T]] {
    def toExpr(x: Array[T]) = '{
      val array = new Array[T](${x.length})(${implicitly[Expr[ClassTag[T]]]})
      ${initArray(x, 'array)}
    }
  }

  implicit def IntArrayIsLiftable: Liftable[Array[Int]] = new Liftable[Array[Int]] {
    def toExpr(x: Array[Int]) = '{
      val array = new Array[Int](${x.length})
      ${initArray(x, 'array)}
    }
  }

  private def initArray[T : Liftable : TypeTag](arr: Array[T], array: Expr[Array[T]])(given QuoteContext): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${i}) = ${x} }
      }.toList,
      array)
  }
}
