import scala.deriving._
import scala.compiletime.erasedValue

inline def summon[T]: T = given match {
  case t: T => t
}

inline def summonAll[T <: Tuple]: List[Eq[_]] = inline erasedValue[T] match {
  case _: Unit => Nil
  case _: (t *: ts) => summon[Eq[t]] :: summonAll[ts]
}

trait Eq[T] {
  def eqv(x: T, y: T): Boolean
}

object Eq {
  given as Eq[Int] {
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

  inline given derived[T] as Eq[T] given (m: Mirror.Of[T]) = {
    val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match {
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
    }
  }
}

enum Opt[+T] derives Eq {
  case Sm(t: T)
  case Nn
}

object Test extends App {
  import Opt._
  val eqoi = the[Eq[Opt[Int]]]
  assert(eqoi.eqv(Sm(23), Sm(23)))
  assert(!eqoi.eqv(Sm(23), Sm(13)))
  assert(!eqoi.eqv(Sm(23), Nn))
}
