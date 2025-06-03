import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

inline def summonAll[T <: Tuple]: List[Eq[_]] = inline erasedValue[T] match {
  case _: EmptyTuple => Nil
  case _: (t *: ts) => summonInline[Eq[t]] :: summonAll[ts]
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

  def eqSum[T](s: Mirror.SumOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean = {
        val ordx = s.ordinal(x)
        (s.ordinal(y) == ordx) && check(elems(ordx))(x, y)
      }
    }

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[Eq[_]]): Eq[T] =
    new Eq[T] {
      def eqv(x: T, y: T): Boolean =
        iterator(x).zip(iterator(y)).zip(elems.iterator).forall {
          case ((x, y), elem) => check(elem)(x, y)
        }
    }

  inline given derived: [T] => (m: Mirror.Of[T]) => Eq[T] = {
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match {
      case s: Mirror.SumOf[T]     => eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => eqProduct(p, elemInstances)
    }
  }
}

enum Tree[T] derives Eq {
  case Branch(left: Tree[T], right: Tree[T])
  case Leaf(elem: T)
}

@main
def Test = {
  import Tree.*

  val t1 = Branch(Leaf(1), Leaf(1))
  assert(summon[Eq[Tree[Int]]].eqv(t1, t1))
}
