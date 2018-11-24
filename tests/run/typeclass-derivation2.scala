import scala.collection.mutable

// Generic deriving infrastructure
object Deriving {

  /** The shape of an ADT in a sum of products representation */
  enum Shape {

    /** A sum with alternative types `Alts` */
    case Cases[Alts <: Tuple]

    /** A product type `T` with element types `Elems` */
    case Case[T, Elems <: Tuple]
  }

  /** A generic representation of a case in an ADT
   *  @param  ordinal   The ordinal value of the case in the list of the ADT's cases
   *  @param  elems     The elements of the case
   */
  class CaseMirror[+T](val ordinal: Int, val elems: Product) {

    /** A generic case with elements given as an array */
    def this(ordinal: Int, elems: Array[AnyRef]) =
      this(ordinal, new ArrayProduct(elems))

    /** A generic case with an initial empty array of `numElems` elements, to be filled in. */
    def this(ordinal: Int, numElems: Int) =
      this(ordinal, new Array[AnyRef](numElems))

    /** A generic case with no elements */
    def this(ordinal: Int) =
      this(ordinal, EmptyProduct)

    /** The `n`'th element of this generic case */
    def apply(n: Int): Any = elems.productElement(n)
  }

  /** Helper class to turn arrays into products */
  private class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  /** Helper object */
  private object EmptyProduct extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = throw new IndexOutOfBoundsException
    def productArity = 0
  }

  /** A class for mapping between an ADT value and
   *  the generic case that represents the value
   */
  abstract class MirrorMapper[T] {
    def toMirror(x: T): CaseMirror[T]
    def fromMirror(c: CaseMirror[T]): T
  }

  /** Every generic derivation starts with a typeclass instance of this type.
   *  It informs that type `T` has shape `S` and is backed by the extended generic mapper.
   */
  abstract class Shaped[T, S <: Shape] extends MirrorMapper[T]
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

  type Shape[T] = Shape.Cases[(
    Shape.Case[Cons[T], (T, Lst[T])],
    Shape.Case[Nil.type, Unit]
  )]

  val NilMirror = new CaseMirror[Nil.type](1)

  implicit def lstShape[T]: Shaped[Lst[T], Shape[T]] = new {
    def toMirror(xs: Lst[T]): CaseMirror[Lst[T]] = xs match {
      case xs: Cons[T] => new CaseMirror[Cons[T]](0, xs)
      case Nil => NilMirror
     }
    def fromMirror(c: CaseMirror[Lst[T]]): Lst[T] = c.ordinal match {
      case 0 => Cons[T](c(0).asInstanceOf, c(1).asInstanceOf)
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

  type Shape[T] = Shape.Case[Pair[T], (T, T)]

  implicit def pairShape[T]: Shaped[Pair[T], Shape[T]] = new {
    def toMirror(xy: Pair[T]) =
      new CaseMirror[Pair[T]](0, xy)
    def fromMirror(c: CaseMirror[Pair[T]]): Pair[T] =
      Pair(c(0).asInstanceOf, c(1).asInstanceOf)
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

  inline def eqlElems[Elems <: Tuple](xs: CaseMirror[_], ys: CaseMirror[_], n: Int): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](xs(n).asInstanceOf, ys(n).asInstanceOf) &&
        eqlElems[elems1](xs, ys, n + 1)
      case _: Unit =>
        true
    }

  inline def eqlCase[T, Elems <: Tuple](mapper: MirrorMapper[T], x: T, y: T) =
    eqlElems[Elems](mapper.toMirror(x), mapper.toMirror(y), 0)

  inline def eqlCases[T, Alts <: Tuple](mapper: MirrorMapper[T], x: T, y: T): Boolean =
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

  inline def derived[T, S <: Shape](implicit ev: Shaped[T, S]) <: Eq[T] = new {
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

  inline def pickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], elems: CaseMirror[_], n: Int): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryPickle[elem](buf, elems(n).asInstanceOf[elem])
        pickleElems[elems1](buf, elems, n + 1)
      case _: Unit =>
    }

  inline def pickleCase[T, Elems <: Tuple](mapper: MirrorMapper[T], buf: mutable.ListBuffer[Int], x: T): Unit =
    pickleElems[Elems](buf, mapper.toMirror(x), 0)

  inline def pickleCases[T, Alts <: Tuple](mapper: MirrorMapper[T], buf: mutable.ListBuffer[Int], x: T, n: Int): Unit =
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

  inline def unpickleCase[T, Elems <: Tuple](mapper: MirrorMapper[T], buf: mutable.ListBuffer[Int], ordinal: Int): T = {
    inline val size = constValue[Tuple.Size[Elems]]
    inline if (size == 0)
      mapper.fromMirror(new CaseMirror[T](ordinal))
    else {
      val elems = new Array[Object](size)
      unpickleElems[Elems](buf, elems, 0)
      mapper.fromMirror(new CaseMirror[T](ordinal, elems))
    }
  }

  inline def unpickleCases[T, Alts <: Tuple](mapper: MirrorMapper[T], buf: mutable.ListBuffer[Int], ordinal: Int, n: Int): T =
    inline erasedValue[Alts] match {
      case _: (Shape.Case[_, elems] *: alts1) =>
        if (n == ordinal) unpickleCase[T, elems](mapper, buf, ordinal)
        else unpickleCases[T, alts1](mapper, buf, ordinal, n + 1)
      case _ =>
        throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ordinal")
    }

  inline def derived[T, S <: Shape](implicit ev: Shaped[T, S]): Pickler[T] = new {
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