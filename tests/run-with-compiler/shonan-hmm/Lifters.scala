
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._

object Lifters {

  implicit def ClassTagIsLiftable[T : Type](implicit ct: ClassTag[T]): Liftable[ClassTag[T]] =
    ct => '(ClassTag(~ct.runtimeClass.toExpr))

  implicit def ArrayIsLiftable[T : Type: ClassTag](implicit l: Liftable[T]): Liftable[Array[T]] = arr => '{
    val array = new Array[T](~arr.length.toExpr)(~implicitly[ClassTag[T]].toExpr)
    ~initArray(arr, '(array))
  }

  implicit def IntArrayIsLiftable: Liftable[Array[Int]] = arr => '{
    val array = new Array[Int](~arr.length.toExpr)
    ~initArray(arr, '(array))
  }

  private def initArray[T : Liftable : Type](arr: Array[T], array: Expr[Array[T]]): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ (~array)(~i.toExpr) = ~x.toExpr }
      }.toList,
      array)
  }

}
