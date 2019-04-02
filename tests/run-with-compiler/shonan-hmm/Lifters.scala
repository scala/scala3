
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._
import scala.quoted.autolift._

object Lifters {
  implicit def LiftedClassTag[T: Type](implicit ct: ClassTag[T]): Expr[ClassTag[T]] = {
    '{ ClassTag(${ct.runtimeClass })}
  }

  implicit def ArrayIsLiftable[T : Type: ClassTag](implicit l: Liftable[T]): Liftable[Array[T]] = arr => '{
    val array = new Array[T](${arr.length})(${implicitly[Expr[ClassTag[T]]]})
    ${initArray(arr, 'array)}
  }

  implicit def IntArrayIsLiftable: Liftable[Array[Int]] = arr => '{
    val array = new Array[Int](${arr.length})
    ${initArray(arr, 'array)}
  }

  private def initArray[T : Liftable : Type](arr: Array[T], array: Expr[Array[T]]): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${i}) = ${x} }
      }.toList,
      array)
  }
}
