package scala
import annotation.showAsInfix
import compiletime._
import internal._

import scala.runtime.DynamicTuple

sealed trait Tuple extends Any {
  import Tuple._

  inline def toArray: Array[Object] =
    DynamicTuple.dynamicToArray(this)

  inline def *: [H, This >: this.type <: Tuple] (x: H): H *: This =
    DynamicTuple.dynamicCons[This, H](this, x)

  inline def ++ [This >: this.type <: Tuple](that: Tuple): Concat[This, that.type] =
    DynamicTuple.dynamicConcat[This, that.type](this, that)

  inline def size[This >: this.type <: Tuple]: Size[This] =
    DynamicTuple.dynamicSize(this)

}

object Tuple {

  type Head[X <: NonEmptyTuple] = X match {
    case x *: _ => x
  }

  type Tail[X <: NonEmptyTuple] <: Tuple = X match {
    case _ *: xs => xs
  }

  type Concat[X <: Tuple, +Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[X <: Tuple, N] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  type Size[X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Size[xs]]
  }

  def fromArray[T](xs: Array[T]): Tuple = {
    val xs2 = xs match {
      case xs: Array[Object] => xs
      case xs => xs.map(_.asInstanceOf[Object])
    }
    DynamicTuple.dynamicFromArray[Tuple](xs2)
  }

  def fromProduct(product: Product): Tuple =
    runtime.DynamicTuple.dynamicFromProduct[Tuple](product)

}

sealed trait NonEmptyTuple extends Tuple {
  import Tuple._

  inline def head[This >: this.type <: NonEmptyTuple]: Head[This] =
    DynamicTuple.dynamicHead[This](this)

  inline def tail[This >: this.type <: NonEmptyTuple]: Tail[This] =
    DynamicTuple.dynamicTail[This](this)

  inline def apply[This >: this.type <: NonEmptyTuple](n: Int): Elem[This, n.type] =
    DynamicTuple.dynamicApply[This, n.type](this, n)
}

@showAsInfix
sealed abstract class *:[+H, +T <: Tuple] extends NonEmptyTuple

object *: {
  inline def unapply[H, T <: Tuple](x: H *: T) = (x.head, x.tail)
}
