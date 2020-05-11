
import UnrolledExpr._

import scala.reflect.ClassTag
import scala.quoted._

object Lifters {
  implicit def LiftedClassTag[T: ClassTag](using s: Scope)(uisng s.Type[T]): s.Expr[ClassTag[T]] = {
    '{ ClassTag(${Expr(summon[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]])}) }
  }

  implicit def ArrayIsLiftable[T: ClassTag](using s: Scope)(using s.Liftable[T], s.Type[T]): s.Liftable[Array[T]] = new s.Liftable[Array[T]] {
    def toExpr(x: Array[T]) = '{
      val array = new Array[T](${Expr(x.length)})(${implicitly[s.Expr[ClassTag[T]]]})
      ${initArray(x, 'array)}
    }
  }

  implicit def IntArrayIsLiftable(using s: Scope): s.Liftable[Array[Int]] = new s.Liftable[Array[Int]] {
    def toExpr(x: Array[Int]) = '{
      val array = new Array[Int](${Expr(x.length)})
      ${initArray(x, 'array)}
    }
  }

  private def initArray[T](using s: Scope)(arr: Array[T], array: s.Expr[Array[T]])(using s.Liftable[T], s.Type[T]): s.Expr[Array[T]] = {
    UnrolledExpr.block(
      arr.zipWithIndex.map[s.Expr[Unit]] {
        case (x, i) => '{ $array(${Expr(i)}) = ${Expr(x)} }
      }.toList,
      array)
  }
}
