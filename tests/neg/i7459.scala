object Foo {
  inline def summon[T](x: T): T =  x match {
    case t: T => t
  }
  println(summon)
}

import scala.deriving.*
import scala.compiletime.erasedValue

inline def summon[T](using t:T): T = t match {
  case t: T => t
}

inline def summonAll[T <: Tuple]: List[Eq[_]] = inline erasedValue[T] match {
  case _: EmptyTuple => Nil
  case _: (t *: ts) => summon[Eq[t]] :: summonAll[ts]  // error
}

trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}

object Eq {
  given Eq[Int] {
    def eqv(x: Int, y: Int) = x == y
  }

  def check(elem: Eq[_])(x: Any, y: Any): Boolean =
    elem.asInstanceOf[Eq[Any]].eqv(x, y)

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def eqSum[T](s: Mirror.SumOf[T], elems: List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = {
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)
      }
    }

  def eqProduct[T](p: Mirror.ProductOf[T], elems: List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean =
        iterator(x).zip(iterator(y)).zip(elems.iterator).forall {
          case ((x, y), elem) => check(elem)(x, y)
        }
    }

  inline given derived[T](using m: Mirror.Of[T]): Eq[T] = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match {
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
    }
  }
}


enum Opt[+T] derives Eq {
  case Sm[T](t: T) extends Opt[T]
  case Nn
}
