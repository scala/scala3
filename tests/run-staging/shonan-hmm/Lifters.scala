
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._

object Lifters {
  implicit def LiftedClassTag[T: Type: ClassTag] (using Quotes): Expr[ClassTag[T]] = {
    '{ ClassTag(${Value(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])}) }
  }

  implicit def ArrayIsToExpr[T : Type: ClassTag](implicit l: ToExpr[T]): ToExpr[Array[T]] = new ToExpr[Array[T]] {
   def apply(x: Array[T])(using Quotes) = '{
      val array = new Array[T](${Value(x.length)})(${implicitly[Expr[ClassTag[T]]]})
      ${initArray(x, 'array)}
    }
  }

  implicit def IntArrayIsToExpr: ToExpr[Array[Int]] = new ToExpr[Array[Int]] {
   def apply(x: Array[Int])(using Quotes) = '{
      val array = new Array[Int](${Value(x.length)})
      ${initArray(x, 'array)}
    }
  }

  private def initArray[T : ToExpr : Type](arr: Array[T], array: Expr[Array[T]])(using Quotes): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${Value(i)}) = ${Value(x)} }
      }.toList,
      array)
  }
}
