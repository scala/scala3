object Deriving {
  import scala.typelevel._

  enum Shape {
    case Sum[Alts <: Tuple]
    case Product[T, Elems <: Tuple]
  }

  case class GenericCase[+T](ordinal: Int, elems: Array[Object])

  abstract class GenericMapper[T] {
    def toGenericCase(x: T): GenericCase[T]
    def fromGenericCase(c: GenericCase[T]): T
  }

  abstract class HasShape[T, S <: Shape] extends GenericMapper[T]

  enum Lst[+T] {
    case Cons(hd: T, tl: Lst[T])
    case Nil
  }

  object Lst {
    type LstShape[T] = Shape.Sum[(
      Shape.Product[Cons[T], (T, Lst[T])],
      Shape.Product[Nil.type, Unit]
    )]

    implicit def lstShape[T]: HasShape[Lst[T], LstShape[T]] = new {
      def toGenericCase(xs: Lst[T]): GenericCase[Lst[T]] = xs match {
        case Cons(x, xs1) => GenericCase[Cons[T]](0, Array(x.asInstanceOf, xs1))
        case Nil => GenericCase[Nil.type](1, Array())
      }
      def fromGenericCase(c: GenericCase[Lst[T]]): Lst[T] = c.ordinal match {
        case 0 => Cons[T](c.elems(0).asInstanceOf, c.elems(1).asInstanceOf)
        case 1 => Nil
      }
    }

    implicit def LstEq[T: Eq]: Eq[Lst[T]] = Eq.derived
  }

  trait Eq[T] {
    def equals(x: T, y: T): Boolean
  }

  object Eq {
    inline def tryEq[T](x: T, y: T) = implicit match {
      case eq: Eq[T] => eq.equals(x, y)
    }

    inline def deriveForSum[T, Alts <: Tuple](mapper: GenericMapper[T], x: T, y: T): Boolean =
      inline erasedValue[Alts] match {
        case _: (Shape.Product[alt, elems] *: alts1) =>
          x match {
            case x: `alt` =>
              y match {
                case y: `alt` => deriveForProduct[T, elems](mapper, x, y)
                case _ => false
              }
          case _ => deriveForSum[T, alts1](mapper, x, y)
        }
      case _: Unit =>
        false
    }

    inline def deriveForProduct[T, Elems <: Tuple](mapper: GenericMapper[T], x: T, y: T) =
      deriveForTuple[Elems](0, mapper.toGenericCase(x).elems, mapper.toGenericCase(y).elems)

    inline def deriveForTuple[Elems <: Tuple](n: Int, xs: Array[Object], ys: Array[Object]): Boolean =
      inline erasedValue[Elems] match {
        case _: (elem *: elems1) =>
          tryEq[elem](xs(n).asInstanceOf, ys(n).asInstanceOf) &&
          deriveForTuple[elems1](n + 1, xs, ys)
        case _: Unit =>
          true
      }

    inline def derived[T, S <: Shape](implicit ev: HasShape[T, S]): Eq[T] = new {
      def equals(x: T, y: T): Boolean = inline erasedValue[S] match {
        case _: Shape.Sum[alts] =>
          deriveForSum[T, alts](ev, x, y)
        case _: Shape.Product[_, elems] =>
          deriveForProduct[T, elems](ev, x, y)
      }
    }

    implicit object eqInt extends Eq[Int] {
      def equals(x: Int, y: Int) = x == y
    }
  }
}

object Test extends App {
  import Deriving._
  val eq = implicitly[Eq[Lst[Int]]]
  val xs = Lst.Cons(1, Lst.Cons(2, Lst.Cons(3, Lst.Nil)))
  val ys = Lst.Cons(1, Lst.Cons(2, Lst.Nil))
  assert(eq.equals(xs, xs))
  assert(!eq.equals(xs, ys))
  assert(!eq.equals(ys, xs))
  assert(eq.equals(ys, ys))

  val eq2 = implicitly[Eq[Lst[Lst[Int]]]]
  val xss = Lst.Cons(xs, Lst.Cons(ys, Lst.Nil))
  val yss = Lst.Cons(xs, Lst.Nil)
  assert(eq2.equals(xss, xss))
  assert(!eq2.equals(xss, yss))
  assert(!eq2.equals(yss, xss))
  assert(eq2.equals(yss, yss))
}