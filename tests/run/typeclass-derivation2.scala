import scala.collection.mutable

// Generic deriving infrastructure
object Deriving {

  enum Shape {
    case Cases[Alts <: Tuple]
    case Case[T, Elems <: Tuple]
  }

  case class GenericCase[+T](ordinal: Int, elems: Array[Object])

  abstract class GenericMapper[T] {
    def toGenericCase(x: T): GenericCase[T]
    def fromGenericCase(c: GenericCase[T]): T
  }

  abstract class HasShape[T, S <: Shape] extends GenericMapper[T]
}

// An algebraic datatype
enum Lst[+T] // derives Eq, Pickler
{
  case Cons(hd: T, tl: Lst[T])
  case Nil
}

object Lst {
  // common compiler-generated infrastructure
  import Deriving._

  type LstShape[T] = Shape.Cases[(
    Shape.Case[Cons[T], (T, Lst[T])],
    Shape.Case[Nil.type, Unit]
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

  // two clauses that could be generated from a `derives` clause
  implicit def LstEq[T: Eq]: Eq[Lst[T]] = Eq.derived
  implicit def LstPickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
}

// A simple product type
case class Pair[T](x: T, y: T) // derives Eq, Pickler

object Pair {
  // common compiler-generated infrastructure
  import Deriving._

  type PairShape[T] = Shape.Case[Pair[T], (T, T)]

  implicit def pairShape[T]: HasShape[Pair[T], PairShape[T]] = new {
    def toGenericCase(xy: Pair[T]) =
      GenericCase[Pair[T]](0, Array(xy._1.asInstanceOf, xy._2.asInstanceOf))
    def fromGenericCase(c: GenericCase[Pair[T]]): Pair[T] =
      Pair(c.elems(0).asInstanceOf, c.elems(1).asInstanceOf)
  }

  // two clauses that could be generated from a `derives` clause
  implicit def PairEq[T: Eq]: Eq[Pair[T]] = Eq.derived
  implicit def PairPickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
}

// A typeclass
trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.typelevel._
  import Deriving._

  inline def tryEql[T](x: T, y: T) = implicit match {
    case eq: Eq[T] => eq.eql(x, y)
  }

  inline def eqlElems[Elems <: Tuple](xs: Array[Object], ys: Array[Object], n: Int): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](xs(n).asInstanceOf, ys(n).asInstanceOf) &&
        eqlElems[elems1](xs, ys, n + 1)
      case _: Unit =>
        true
    }

  inline def eqlCase[T, Elems <: Tuple](mapper: GenericMapper[T], x: T, y: T) =
    eqlElems[Elems](mapper.toGenericCase(x).elems, mapper.toGenericCase(y).elems, 0)

  inline def eqlCases[T, Alts <: Tuple](mapper: GenericMapper[T], x: T, y: T): Boolean =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[alt, elems] *: alts1) =>
        x match {
          case x: `alt` =>
            y match {
              case y: `alt` => eqlCase[T, elems](mapper, x, y)
              case _ => false
            }
          case _ => eqlCases[T, alts1](mapper, x, y)
      }
    case _: Unit =>
      false
  }

  inline def derived[T, S <: Shape](implicit ev: HasShape[T, S]) <: Eq[T] = new {
    def eql(x: T, y: T): Boolean = inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        eqlCases[T, alts](ev, x, y)
      case _: Shape.Case[_, elems] =>
        eqlCase[T, elems](ev, x, y)
    }
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }
}

// Another typeclass
trait Pickler[T] {
  def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
  def unpickle(buf: mutable.ListBuffer[Int]): T
}

object Pickler {
  import scala.typelevel._
  import Deriving._

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = implicit match {
    case pkl: Pickler[T] => pkl.pickle(buf, x)
  }

  inline def pickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: Array[AnyRef], n: Int): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryPickle[elem](buf, elems(n).asInstanceOf[elem])
        pickleElems[elems1](buf, elems, n + 1)
      case _: Unit =>
    }

  inline def pickleCase[T, Elems <: Tuple](mapper: GenericMapper[T], buf: mutable.ListBuffer[Int], x: T): Unit =
    pickleElems[Elems](buf, mapper.toGenericCase(x).elems, 0)

  inline def pickleCases[T, Alts <: Tuple](mapper: GenericMapper[T], buf: mutable.ListBuffer[Int], x: T, n: Int): Unit =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[alt, elems] *: alts1) =>
        x match {
          case x: `alt` =>
            buf += n
            pickleCase[T, elems](mapper, buf, x)
          case _ =>
            pickleCases[T, alts1](mapper, buf, x, n + 1)
        }
      case _: Unit =>
    }

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = implicit match {
    case pkl: Pickler[T] => pkl.unpickle(buf)
  }

  inline def unpickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: Array[AnyRef], n: Int): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        elems(n) = tryUnpickle[elem](buf).asInstanceOf[AnyRef]
        unpickleElems[elems1](buf, elems, n + 1)
      case _: Unit =>
    }

  inline def unpickleCase[T, Elems <: Tuple](mapper: GenericMapper[T], buf: mutable.ListBuffer[Int], ordinal: Int): T = {
    val elems = new Array[Object](constValue[Tuple.Size[Elems]])
    unpickleElems[Elems](buf, elems, 0)
    mapper.fromGenericCase(GenericCase(ordinal, elems))
  }

  inline def unpickleCases[T, Alts <: Tuple](mapper: GenericMapper[T], buf: mutable.ListBuffer[Int], ordinal: Int, n: Int): T =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[_, elems] *: alts1) =>
        if (n == ordinal) unpickleCase[T, elems](mapper, buf, ordinal)
        else unpickleCases[T, alts1](mapper, buf, ordinal, n + 1)
      case _ =>
        throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ordinal")
    }

  inline def derived[T, S <: Shape](implicit ev: HasShape[T, S]): Pickler[T] = new {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit = inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        pickleCases[T, alts](ev, buf, x, 0)
      case _: Shape.Case[_, elems] =>
        pickleCase[T, elems](ev, buf, x)
    }
    def unpickle(buf: mutable.ListBuffer[Int]): T = inline erasedValue[S] match {
      case _: Shape.Cases[alts] =>
        unpickleCases[T, alts](ev, buf, nextInt(buf), 0)
      case _: Shape.Case[_, elems] =>
        unpickleCase[T, elems](ev, buf, 0)
    }
  }

  implicit object IntPickler extends Pickler[Int] {
    def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
    def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
  }
}

// Tests
object Test extends App {
  import Deriving._
  val eq = implicitly[Eq[Lst[Int]]]
  val xs = Lst.Cons(11, Lst.Cons(22, Lst.Cons(33, Lst.Nil)))
  val ys = Lst.Cons(11, Lst.Cons(22, Lst.Nil))
  assert(eq.eql(xs, xs))
  assert(!eq.eql(xs, ys))
  assert(!eq.eql(ys, xs))
  assert(eq.eql(ys, ys))

  val eq2 = implicitly[Eq[Lst[Lst[Int]]]]
  val xss = Lst.Cons(xs, Lst.Cons(ys, Lst.Nil))
  val yss = Lst.Cons(xs, Lst.Nil)
  assert(eq2.eql(xss, xss))
  assert(!eq2.eql(xss, yss))
  assert(!eq2.eql(yss, xss))
  assert(eq2.eql(yss, yss))

  val buf = new mutable.ListBuffer[Int]
  val pkl = implicitly[Pickler[Lst[Int]]]
  pkl.pickle(buf, xs)
  println(buf)
  val xs1 = pkl.unpickle(buf)
  println(xs1)
  assert(xs1 == xs)
  assert(eq.eql(xs1, xs))

  val pkl2 = implicitly[Pickler[Lst[Lst[Int]]]]
  pkl2.pickle(buf, xss)
  println(buf)
  val xss1 = pkl2.unpickle(buf)
  println(xss1)
  assert(xss == xss1)
  assert(eq2.eql(xss, xss1))

  val p1 = Pair(1, 2)
  val p2 = Pair(1, 2)
  val p3 = Pair(2, 1)
  val eqp = implicitly[Eq[Pair[Int]]]
  assert(eqp.eql(p1, p2))
  assert(!eqp.eql(p2, p3))

  val pklp = implicitly[Pickler[Pair[Int]]]
  pklp.pickle(buf, p1)
  println(buf)
  val p1a = pklp.unpickle(buf)
  println(p1a)
  assert(p1 == p1a)
  assert(eqp.eql(p1, p1a))
}