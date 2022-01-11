import scala.collection.mutable
import scala.annotation.tailrec
import scala.compiletime.summonInline

// Simulation of an alternative type class derivation scheme proposed in #6153

// -- Classes and Objects of the Derivation Framework ----------------------------------

/** Core classes. In the current implementation these are in the scala.reflect package */
object Deriving {

  /** The Generic class hierarchy allows typelevel access to
   *  enums, case classes and objects, and their sealed parents.
   */
  sealed abstract class Generic[T]
  object Generic {

    /** The Generic for a sum type */
    abstract class Sum[T] extends Generic[T] {

      /** The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal` */
      def ordinal(x: T): Int

      /** The number of cases in the sum.
      *  Implemented by an inline method in concrete subclasses.
      */
      erased def numberOfCases: Int

      /** The Generic representations of the sum's alternatives.
      *  Implemented by an inline method in concrete subclasses.
      */
      erased def alternative(n: Int): Generic[_ <: T]
    }

    /** The Generic for a product type */
    abstract class Product[T] extends Generic[T] {

      /** The types of the elements */
      type ElemTypes <: Tuple

      /** The name of the whole product type */
      type CaseLabel <: String

      /** The names of the product elements */
      type ElemLabels <: Tuple

      /** Create a new instance of type `T` with elements taken from product `p`. */
      def fromProduct(p: scala.Product): T
    }

    /** The Generic for a singleton */
    trait Singleton[T] extends Generic[T] {

      /** The name of the singleton */
      type CaseLabel <: String

      /** The represented value */
      inline def singletonValue = summonInline[ValueOf[T]].value
    }
  }

  /** Helper class to turn arrays into products */
  class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  /** Helper method to select a product element */
  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]
}
import Deriving.*

// -- Example Datatypes ---------------------------------------------------------

// Everthing except for the base traits and their cases is supposed to be compiler-generated.

sealed trait Lst[+T] // derives Eq, Pickler, Show

object Lst {

  class GenericLst[T] extends Generic.Sum[Lst[T]] {
    def ordinal(x: Lst[T]) = x match {
      case x: Cons[_] => 0
      case Nil => 1
    }
    inline override def numberOfCases = 2
    transparent inline override def alternative(n: Int): Generic[_ <: Lst[T]] =
      inline n match {
        case 0 => Cons.GenericCons[T]
        case 1 => Nil.GenericNil
      }
  }

  implicit def GenericLst[T]: GenericLst[T] = new GenericLst[T]

  case class Cons[T](hd: T, tl: Lst[T]) extends Lst[T]

  object Cons extends Generic.Product[Cons[_]] {
    def apply[T](x: T, xs: Lst[T]): Lst[T] = new Cons(x, xs)

    def fromProduct(p: Product): Cons[_] =
      new Cons(productElement[Any](p, 0), productElement[Lst[Any]](p, 1))

    implicit def GenericCons[T]: Generic.Product[Cons[T]] {
      type ElemTypes = (T, Lst[T])
      type CaseLabel = "Cons"
      type ElemLabels = ("hd", "tl")
    } = this.asInstanceOf
  }

  case object Nil extends Lst[Nothing] with Generic.Singleton[Nil.type] {
    type CaseLabel = "Nil"
    implicit def GenericNil: Nil.type = this
  }

  // three clauses that would be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Lst[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Lst[T]] = Show.derived
}

// A simple product type
case class Pair[T](x: T, y: T) // derives Eq, Pickler, Show

object Pair extends Generic.Product[Pair[_]] {

  def fromProduct(p: Product): Pair[_] =
    Pair(productElement[Any](p, 0), productElement[Any](p, 1))

  implicit def GenericPair[T]: Generic.Product[Pair[T]] {
    type ElemTypes = (T, T)
    type CaseLabel = "Pair"
    type ElemLabels = ("x", "y")
  } = this.asInstanceOf

  // clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Pair[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Pair[T]] = Show.derived
}

// Another sum type
sealed trait Either[+L, +R] extends Product with Serializable // derives Eq, Pickler, Show

object Either {
  class GenericEither[L, R] extends Generic.Sum[Either[L, R]] {
    def ordinal(x: Either[L, R]) = x match {
      case x: Left[_] => 0
      case x: Right[_] => 1
    }
    inline override def numberOfCases = 2
    inline override def alternative(n: Int): _ <: Generic[_ <: Either[L, R]] =
      inline n match {
        case 0 => Left.GenericLeft[L]
        case 1 => Right.GenericRight[R]
      }
  }
  implicit def GenericEither[L, R]: GenericEither[L, R] = new GenericEither[L, R]

  implicit def derived$Eq[L: Eq, R: Eq]: Eq[Either[L, R]] = Eq.derived
  implicit def derived$Pickler[L: Pickler, R: Pickler]: Pickler[Either[L, R]] = Pickler.derived
  implicit def derived$Show[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

case class Left[L](elem: L) extends Either[L, Nothing]
case class Right[R](elem: R) extends Either[Nothing, R]

object Left extends Generic.Product[Left[_]] {
  def fromProduct(p: Product): Left[_] = Left(productElement[Any](p, 0))
  implicit def GenericLeft[L]: Generic.Product[Left[L]] {
    type ElemTypes = L *: EmptyTuple
    type CaseLabel = "Left"
    type ElemLabels = "x" *: EmptyTuple
  } = this.asInstanceOf
}

object Right extends Generic.Product[Right[_]] {
  def fromProduct(p: Product): Right[_] = Right(productElement[Any](p, 0))
  implicit def GenericRight[R]: Generic.Product[Right[R]] {
    type ElemTypes = R *: EmptyTuple
    type CaseLabel = "Right"
    type ElemLabels = "x" *: EmptyTuple
  } = this.asInstanceOf
}

// -- Type classes ------------------------------------------------------------

// Everything here is hand-written by the authors of the derivable type classes
// The same schema is used throughout.
//
//  - A type class implements an inline `derived` method, given a `Generic` instance.
//  - Each implemented type class operation `xyz` calls 4 inline helper methods:
//      1. `xyzCases` for sums,
//      2. `xyzProduct` for products,
//      3. `xyzElems` stepping through the elements of a product,
//      4. `tryXyz` for searching the implicit to handles a single element.
//  - The first three methods have two parameter lists. The first parameter
//    list contains inline parameters that guide the code generation, whereas
//    the second parameter list contains parameters that show up in the
//    generated code. (This is done just to make things clearer).

// Equality type class
trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.erasedValue

  inline def tryEql[T](x: T, y: T) = summonInline[Eq[T]].eql(x, y)

  inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
        eqlElems[elems1](n + 1)(x, y)
      case _: EmptyTuple =>
        true
    }

  inline def eqlProduct[T](g: Generic.Product[T])(x: Any, y: Any): Boolean =
    eqlElems[g.ElemTypes](0)(x, y)

  inline def eqlCases[T](g: Generic.Sum[T], n: Int)(x: T, y: T, ord: Int): Boolean =
    inline if (n == g.numberOfCases)
      false
    else if (ord == n)
      inline g.alternative(n) match {
        case g: Generic.Product[p] => eqlProduct[p](g)(x, y)
        case g: Generic.Singleton[_] => true
      }
    else eqlCases[T](g, n + 1)(x, y, ord)

  inline def derived[T](implicit ev: Generic[T]): Eq[T] = new Eq[T] {
    def eql(x: T, y: T): Boolean =
      inline ev match {
        case g: Generic.Sum[T] =>
          val ord = g.ordinal(x)
          ord == g.ordinal(y) && eqlCases[T](g, 0)(x, y, ord)
        case g: Generic.Product[T] =>
          eqlProduct[T](g)(x, y)
        case g: Generic.Singleton[_] =>
          true
      }
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }
}

// Pickling type class
trait Pickler[T] {
  def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
  def unpickle(buf: mutable.ListBuffer[Int]): T
}

object Pickler {
  import scala.compiletime.{erasedValue, constValue}

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit =
    summonInline[Pickler[T]].pickle(buf, x)

  inline def pickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryPickle[elem](buf, productElement[elem](x, n))
        pickleElems[elems1](n + 1)(buf, x)
      case _: EmptyTuple =>
    }

  inline def pickleProduct[T](g: Generic.Product[T])(buf: mutable.ListBuffer[Int], x: Any): Unit =
    pickleElems[g.ElemTypes](0)(buf, x)

  inline def pickleCases[T](g: Generic.Sum[T], inline n: Int)(buf: mutable.ListBuffer[Int], x: T, ord: Int): Unit =
    inline if (n == g.numberOfCases)
      ()
    else if (ord == n)
      inline g.alternative(n) match {
        case g: Generic.Product[p] => pickleProduct(g)(buf, x)
        case g: Generic.Singleton[s] =>
      }
    else pickleCases[T](g, n + 1)(buf, x, ord)

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = summonInline[Pickler[T]].unpickle(buf)

  inline def unpickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], elems: Array[AnyRef]): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        elems(n) = tryUnpickle[elem](buf).asInstanceOf[AnyRef]
        unpickleElems[elems1](n + 1)(buf, elems)
      case _: EmptyTuple =>
    }

  inline def unpickleProduct[T](g: Generic.Product[T])(buf: mutable.ListBuffer[Int]): T = {
    // inline val size = constValue[Tuple.Size[g.ElemTypes]]
    // val elems = new Array[Object](size)
    val elems = new Array[Object](buf.size)
    unpickleElems[g.ElemTypes](0)(buf, elems)
    g.fromProduct(ArrayProduct(elems))
  }

  inline def unpickleCases[T](g: Generic.Sum[T], n: Int)(buf: mutable.ListBuffer[Int], ord: Int): T =
    inline if (n == g.numberOfCases)
      throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ord")
    else if (ord == n)
      inline g.alternative(n) match {
        case g: Generic.Product[p] => unpickleProduct(g)(buf)
        case g: Generic.Singleton[s] => g.singletonValue
      }
    else unpickleCases[T](g, n + 1)(buf, ord)

  inline def derived[T](implicit ev: Generic[T]): Pickler[T] = new {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit =
      inline ev match {
        case g: Generic.Sum[T] =>
          val ord = g.ordinal(x)
          buf += ord
          pickleCases[T](g, 0)(buf, x, ord)
        case g: Generic.Product[p] =>
          pickleProduct(g)(buf, x)
        case g: Generic.Singleton[_] =>
      }
    def unpickle(buf: mutable.ListBuffer[Int]): T =
      inline ev match {
        case g: Generic.Sum[T] =>
          unpickleCases[T](g, 0)(buf, nextInt(buf))
        case g: Generic.Product[T] =>
          unpickleProduct[T](g)(buf)
        case g: Generic.Singleton[s] =>
          constValue[s]
      }
  }

  implicit object IntPickler extends Pickler[Int] {
    def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
    def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
  }
}

// Display type class, making use of label info.
trait Show[T] {
  def show(x: T): String
}
object Show {
  import scala.compiletime.{erasedValue, constValue}

  inline def tryShow[T](x: T): String = summonInline[Show[T]].show(x)

  inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val formal = constValue[label]
            val actual = tryShow(productElement[elem](x, n))
            s"$formal = $actual" :: showElems[elems1, labels1](n + 1)(x)
        }
      case _: EmptyTuple =>
        Nil
    }

  inline def showProduct[T](g: Generic.Product[T])(x: Any): String = {
    val labl = constValue[g.CaseLabel]
    showElems[g.ElemTypes, g.ElemLabels](0)(x).mkString(s"$labl(", ", ", ")")
  }

  inline def showCases[T](g: Generic.Sum[T], n: Int)(x: T, ord: Int): String =
    inline if (n == g.numberOfCases)
      ""
    else if (ord == n)
      inline g.alternative(n) match {
        case g: Generic.Product[p] => showProduct(g)(x)
        case g: Generic.Singleton[s] => constValue[g.CaseLabel]
      }
    else showCases[T](g, n + 1)(x, ord)

  inline def derived[T](implicit ev: Generic[T]): Show[T] = new {
    def show(x: T): String =
      inline ev match {
        case g: Generic.Sum[T] =>
          showCases(g, 0)(x, g.ordinal(x))
        case g: Generic.Product[p] =>
          showProduct(g)(x)
        case g: Generic.Singleton[s] =>
          constValue[g.CaseLabel]
      }
  }

  implicit object IntShow extends Show[Int] {
    def show(x: Int): String = x.toString
  }
}

// -- Tests ----------------------------------------------------------------------

object Test extends App {
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

  def showPrintln[T: Show](x: T): Unit =
    println(implicitly[Show[T]].show(x))

  showPrintln(xs)
  showPrintln(xss)

  val zs = Lst.Cons(Left(1), Lst.Cons(Right(Pair(2, 3)), Lst.Nil))
  showPrintln(zs)

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
  showPrintln(zs1)
  assert(eql(zs, zs1))
}
