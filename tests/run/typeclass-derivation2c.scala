import scala.collection.mutable
import scala.annotation.tailrec

// Simulation of an alternative typeclass derivation scheme proposed in #6153
object TypeLevel {

  object EmptyProduct extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = throw new IndexOutOfBoundsException
    def productArity = 0
  }

  /** Helper class to turn arrays into products */
  class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  abstract class Generic[T]

  abstract class GenericSum[T] extends Generic[T] {
    def ordinal(x: T): Int
    def numberOfCases: Int = ???
    def alternative(n: Int): GenericProduct[_ <: T] = ???
  }

  abstract class GenericProduct[T] extends Generic[T] {
    type ElemTypes <: Tuple
    def toProduct(x: T): Product
    def fromProduct(p: Product): T
  }
}

// An algebraic datatype
sealed trait Lst[+T] // derives Eq, Pickler, Show

object Lst {
  // common compiler-generated infrastructure
  import TypeLevel._

  class GenericLst[T] extends GenericSum[Lst[T]] {
    def ordinal(x: Lst[T]) = x match {
      case x: Cons[_] => 0
      case Nil => 1
    }
    inline override def numberOfCases = 2
    inline override def alternative(n: Int) <: GenericProduct[_ <: Lst[T]] =
      inline n match {
        case 0 => Cons.GenericCons[T]
        case 1 => Nil.GenericNil
      }
  }

  implicit def GenericLst[T]: GenericLst[T] = new GenericLst[T]

  case class Cons[T](hd: T, tl: Lst[T]) extends Lst[T]

  object Cons {
    def apply[T](x: T, xs: Lst[T]): Lst[T] = new Cons(x, xs)

    class GenericCons[T] extends GenericProduct[Cons[T]] {
      type ElemTypes = (T, Lst[T])
      def toProduct(x: Cons[T]): Product = x
      def fromProduct(p: Product): Cons[T] =
        new Cons(p.productElement(0).asInstanceOf[T],
                 p.productElement(1).asInstanceOf[Lst[T]])
    }
    implicit def GenericCons[T]: GenericCons[T] = new GenericCons[T]
  }
  case object Nil extends Lst[Nothing] {
    class GenericNil extends GenericProduct[Nil.type] {
      type ElemTypes = Unit
      def toProduct(x: Nil.type): Product = EmptyProduct
      def fromProduct(p: Product): Nil.type = Nil
    }
    implicit def GenericNil: GenericNil = new GenericNil
  }

  // three clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Lst[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
  //implicit def derived$Show[T: Show]: Show[Lst[T]] = Show.derived
}

// A typeclass
trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.erasedValue
  import TypeLevel._

  inline def tryEql[T](x: T, y: T) = implicit match {
    case eq: Eq[T] => eq.eql(x, y)
  }

  inline def eqlElems[Elems <: Tuple](x: Product, y: Product, n: Int): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](
          x.productElement(n).asInstanceOf[elem],
          y.productElement(n).asInstanceOf[elem]) &&
        eqlElems[elems1](x, y, n + 1)
      case _: Unit =>
        true
    }

  inline def eqlCases[T](x: T, y: T, genSum: GenericSum[T], ord: Int, inline n: Int): Boolean =
    inline if (n == genSum.numberOfCases)
      false
    else if (ord == n)
      inline genSum.alternative(n) match {
        case cas: GenericProduct[p] =>
          eqlElems[cas.ElemTypes](
            cas.toProduct(x.asInstanceOf[p]),
            cas.toProduct(y.asInstanceOf[p]),
            0)
      }
    else eqlCases[T](x, y, genSum, ord, n + 1)

  inline def derived[T](implicit ev: Generic[T]): Eq[T] = new Eq[T] {
    def eql(x: T, y: T): Boolean = {
      inline ev match {
        case evv: GenericSum[T] =>
          val ord = evv.ordinal(x)
          ord == evv.ordinal(y) && eqlCases[T](x, y, evv, ord, 0)
        case evv: GenericProduct[T] =>
          eqlElems[evv.ElemTypes](evv.toProduct(x), evv.toProduct(y), 0)
      }
    }
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }
}

// A simple product type
case class Pair[T](x: T, y: T) // derives Eq, Pickler, Show

object Pair {
  // common compiler-generated infrastructure
  import TypeLevel._

  class GenericPair[T] extends GenericProduct[Pair[T]] {
    type ElemTypes = (T, T)
    def toProduct(x: Pair[T]): Product = x
    def fromProduct(p: Product): Pair[T] =
      Pair(p.productElement(0).asInstanceOf, p.productElement(1).asInstanceOf)
  }
  implicit def GenericPair[T]: GenericPair[T] = new GenericPair[T]

  // clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Pair[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
  //implicit def derived$Show[T: Show]: Show[Pair[T]] = Show.derived
}

sealed trait Either[+L, +R] extends Product with Serializable // derives Eq, Pickler, Show

object Either {
  import TypeLevel._

  class GenericEither[L, R] extends GenericSum[Either[L, R]] {
    def ordinal(x: Either[L, R]) = x match {
      case x: Left[L] => 0
      case x: Right[R] => 1
    }
    inline override def numberOfCases = 2
    inline override def alternative(n: Int) <: GenericProduct[_ <: Either[L, R]] =
      inline n match {
        case 0 => Left.GenericLeft[L]
        case 1 => Right.GenericRight[R]
      }
  }
  implicit def GenericEither[L, R]: GenericEither[L, R] = new GenericEither[L, R]

  implicit def derived$Eq[L: Eq, R: Eq]: Eq[Either[L, R]] = Eq.derived
  implicit def derived$Pickler[L: Pickler, R: Pickler]: Pickler[Either[L, R]] = Pickler.derived
  //implicit def derived$Show[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

case class Left[L](elem: L) extends Either[L, Nothing]
case class Right[R](elem: R) extends Either[Nothing, R]

object Left {
  import TypeLevel._
  class GenericLeft[L] extends GenericProduct[Left[L]] {
    type ElemTypes = L *: Unit
    def toProduct(x: Left[L]) = x
    def fromProduct(p: Product): Left[L] = Left(p.productElement(0).asInstanceOf[L])
  }
  implicit def GenericLeft[L]: GenericLeft[L] = new GenericLeft[L]
}

object Right {
  import TypeLevel._
  class GenericRight[R] extends GenericProduct[Right[R]] {
    type ElemTypes = R *: Unit
    def toProduct(x: Right[R]) = x
    def fromProduct(p: Product): Right[R] = Right(p.productElement(0).asInstanceOf[R])
  }
  implicit def GenericRight[R]: GenericRight[R] = new GenericRight[R]
}

// Another typeclass
trait Pickler[T] {
  def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
  def unpickle(buf: mutable.ListBuffer[Int]): T
}

object Pickler {
  import scala.compiletime.{erasedValue, constValue}
  import TypeLevel._

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = implicit match {
    case pkl: Pickler[T] => pkl.pickle(buf, x)
  }

  inline def pickleElems[Elems <: Tuple](buf: mutable.ListBuffer[Int], x: Product, n: Int): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryPickle[elem](buf, x.productElement(n).asInstanceOf[elem])
        pickleElems[elems1](buf, x, n + 1)
      case _: Unit =>
    }

  inline def pickleCases[T](buf: mutable.ListBuffer[Int], x: T, genSum: GenericSum[T], ord: Int, inline n: Int): Unit =
    inline if (n == genSum.numberOfCases)
      ()
    else if (ord == n)
      inline genSum.alternative(n) match {
        case cas: GenericProduct[p] =>
          pickleElems[cas.ElemTypes](buf, cas.toProduct(x.asInstanceOf[p]), 0)
      }
    else pickleCases[T](buf, x, genSum, ord, n + 1)

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

  inline def unpickleCase[T](buf: mutable.ListBuffer[Int], genProd: GenericProduct[T]): T = {
    inline val size = constValue[Tuple.Size[genProd.ElemTypes]]
    inline if (size == 0)
      genProd.fromProduct(EmptyProduct)
    else {
      val elems = new Array[Object](size)
      unpickleElems[genProd.ElemTypes](buf, elems, 0)
      genProd.fromProduct(ArrayProduct(elems))
    }
  }

  inline def unpickleCases[T](buf: mutable.ListBuffer[Int], genSum: GenericSum[T], ord: Int, n: Int): T =
    inline if (n == genSum.numberOfCases)
      throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ord")
    else if (ord == n)
      inline genSum.alternative(n) match {
        case cas: GenericProduct[p] => unpickleCase(buf, cas)
      }
    else unpickleCases[T](buf, genSum, ord, n + 1)

  inline def derived[T](implicit ev: Generic[T]): Pickler[T] = new {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit =
      inline ev match {
        case ev: GenericSum[T] =>
          val ord = ev.ordinal(x)
          buf += ord
          pickleCases[T](buf, x, ev, ord, 0)
        case ev: GenericProduct[p] =>
          pickleElems[ev.ElemTypes](buf, ev.toProduct(x.asInstanceOf[p]), 0)
      }
    def unpickle(buf: mutable.ListBuffer[Int]): T =
      inline ev match {
        case ev: GenericSum[T] =>
          unpickleCases[T](buf, ev, nextInt(buf), 0)
        case ev: GenericProduct[T] =>
          unpickleCase[T](buf, ev)
      }
  }

  implicit object IntPickler extends Pickler[Int] {
    def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
    def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
  }
}

// Tests
object Test extends App {
  import TypeLevel._
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

  val zs = Lst.Cons(Left(1), Lst.Cons(Right(Pair(2, 3)), Lst.Nil))

  def pickle[T: Pickler](buf: mutable.ListBuffer[Int], x: T): Unit =
    implicitly[Pickler[T]].pickle(buf, x)

  def unpickle[T: Pickler](buf: mutable.ListBuffer[Int]): T =
    implicitly[Pickler[T]].unpickle(buf)

  def copy[T: Pickler](x: T): T = {
    val buf = new mutable.ListBuffer[Int]
    pickle(buf, x)
    unpickle[T](buf)
  }

  def eql[T: Eq](x: T, y: T) = implicitly[Eq[T]].eql(x, y)

  val zs1 = copy(zs)
  assert(eql(zs, zs1))
}
