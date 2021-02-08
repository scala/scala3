
import UnrolledExpr.*

import scala.reflect.ClassTag
import scala.quoted.*

object Lifters {
  implicit def LiftedClassTag[T: Type: ClassTag] (using Quotes): Expr[ClassTag[T]] = {
    '{ ClassTag(${Expr(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])}) }
  }

  implicit def ArrayIsToExpr[T : Type: ClassTag](implicit l: ToExpr[T]): ToExpr[Array[T]] = new ToExpr[Array[T]] {
   def apply(x: Array[T])(using Quotes) = '{
      val array = new Array[T](${Expr(x.length)})(${implicitly[Expr[ClassTag[T]]]})
      ${initArray(x, 'array)}
    }
  }

  implicit def IntArrayIsToExpr: ToExpr[Array[Int]] = new ToExpr[Array[Int]] {
   def apply(x: Array[Int])(using Quotes) = '{
      val array = new Array[Int](${Expr(x.length)})
      ${initArray(x, 'array)}
    }
  }

  private def initArray[T : ToExpr : Type](arr: Array[T], array: Expr[Array[T]])(using Quotes): Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map {
        case (x, i) => '{ $array(${Expr(i)}) = ${Expr(x)} }
      }.toList,
      array)
  }
}
