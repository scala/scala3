
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._
import scala.quoted.autolift._

object Lifters {

  implicit def ClassTagIsLiftable[T : Type](implicit ct: ClassTag[T]): Liftable[ClassTag[T]] = new Liftable[ClassTag[T]] {
    override def toExpr(ct: ClassTag[T])(implicit st: StagingContext): Expr[ClassTag[T]] = '{ ClassTag(${ct.runtimeClass })}
  }

  implicit def ArrayIsLiftable[T : Type: ClassTag](implicit l: Liftable[T]): Liftable[Array[T]] = new Liftable[Array[T]] {
    override def toExpr(arr: Array[T])(implicit st: StagingContext): Expr[Array[T]] = '{
      val array = new Array[T](${arr.length})(${implicitly[Expr[ClassTag[T]]]})
      ${initArray(arr, 'array)}
    }
  }

  implicit def IntArrayIsLiftable: Liftable[Array[Int]] = new Liftable[Array[Int]] {
    override def toExpr(arr: Array[Int])(implicit st: StagingContext): Expr[Array[Int]] = '{
      val array = new Array[Int](${arr.length})
      ${initArray(arr, 'array)}
    }
  }

  private def initArray[T : Liftable : Type](arr: Array[T], array: Expr[Array[T]])(implicit st: StagingContext): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${i}) = ${x} }
      }.toList,
      array)
  }
}
