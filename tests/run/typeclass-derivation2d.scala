import scala.collection.mutable
import scala.annotation.tailrec

// Simulation of an alternative typeclass derivation scheme

// -- Classes and Objects of the Derivation Framework ----------------------------------

//** Core classes. In the current implementation these are in the scala.reflect package */
object Deriving {

  /** The Generic class hierarchy allows typelevel access to
   *  enums, case classes and objects, and their sealed parents.
   */
  sealed abstract class Mirror {

    /** The mirrored *-type */
    type _MonoType
  }

  object Mirror {

    /** The Mirror for a sum type */
    trait Sum extends Mirror { self =>

      type ElemTypes <: Tuple

      /** The ordinal number of the case class of `x`. For enums, `ordinal(x) == x.ordinal` */
      def ordinal(x: _MonoType): Int
    }

    /** The Mirror for a product type */
    trait Product extends Mirror {

      /** The types of the elements */
      type ElemTypes <: Tuple

      /** The name of the whole product type */
      type CaseLabel <: String

      /** The names of the product elements */
      type ElemLabels <: Tuple

      /** Create a new instance of type `T` with elements taken from product `p`. */
      def _fromProduct(p: scala.Product): _MonoType
    }

    trait Singleton extends Product {
      type _MonoType = this.type
      def _fromProduct(p: scala.Product) = this
    }
    type Of[T]        = Mirror { type _MonoType = T }
    type ProductOf[T] = Mirror.Product { type _MonoType = T }
    type SumOf[T]     = Mirror.Sum { type _MonoType = T }
   }

  /** Helper class to turn arrays into products */
  class ArrayProduct(val elems: Array[AnyRef]) extends Product {
    def this(size: Int) = this(new Array[AnyRef](size))
    def canEqual(that: Any): Boolean = true
    def productElement(n: Int) = elems(n)
    def productArity = elems.length
    override def productIterator: Iterator[Any] = elems.iterator
    def update(n: Int, x: Any) = elems(n) = x.asInstanceOf[AnyRef]
  }

  object EmptyProduct extends ArrayProduct(Array[AnyRef]())

  /** Helper method to select a product element */
  def productElement[T](x: Any, idx: Int) =
    x.asInstanceOf[Product].productElement(idx).asInstanceOf[T]
}
import Deriving._

sealed trait Lst[+T] // derives Eq, Pickler, Show

object Lst extends Mirror.Sum {
  type _MonoType = Lst[_]

  def ordinal(x: Lst[_]) = x match {
    case x: Cons[_] => 0
    case Nil => 1
  }

  implicit def mirror[T]: Mirror.Sum {
    type _MonoType = Lst[T]
    type ElemTypes = (Cons[T], Nil.type)
  } = this.asInstanceOf

  case class Cons[T](hd: T, tl: Lst[T]) extends Lst[T]

  object Cons extends Mirror.Product {
    type _MonoType = Lst[_]

    def apply[T](x: T, xs: Lst[T]): Lst[T] = new Cons(x, xs)

    def _fromProduct(p: Product): Cons[_] =
      new Cons(productElement[Any](p, 0), productElement[Lst[Any]](p, 1))

    implicit def mirror[T]: Mirror.Product {
      type _MonoType = Cons[T]
      type ElemTypes = (T, Lst[T])
      type CaseLabel = "Cons"
      type ElemLabels = ("hd", "tl")
    } = this.asInstanceOf
  }

  case object Nil extends Lst[Nothing] with Mirror.Singleton {

    implicit def mirror: Mirror.Singleton {
      type _MonoType = Nil.type
      type ElemTypes = Unit
      type CaseLabel = "Nil"
      type ElemLabels = Unit
    } = this.asInstanceOf
  }

  // three clauses that would be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Lst[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Lst[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Lst[T]] = Show.derived
}

// --------------- A simple product type ------------------------

case class Pair[T](x: T, y: T) // derives Eq, Pickler, Show

object Pair extends Mirror.Product {
  type _MonoType = Pair[_]

  def _fromProduct(p: Product): Pair[_] =
    Pair(productElement[Any](p, 0), productElement[Any](p, 1))

  implicit def mirror[T]: Mirror.Product {
    type _MonoType = Pair[T]
    type ElemTypes = (T, T)
    type CaseLabel = "Pair"
    type ElemLabels = ("x", "y")
  } = this.asInstanceOf

  // clauses that could be generated from a `derives` clause
  implicit def derived$Eq[T: Eq]: Eq[Pair[T]] = Eq.derived
  implicit def derived$Pickler[T: Pickler]: Pickler[Pair[T]] = Pickler.derived
  implicit def derived$Show[T: Show]: Show[Pair[T]] = Show.derived
}

// ----------- Another sum type ----------------------------------------

sealed trait Either[+L, +R] extends Product with Serializable // derives Eq, Pickler, Show

object Either extends Mirror.Sum {
  type _MonoType = Either[_, _]

  def ordinal(x: Either[_, _]) = x match {
    case x: Left[_] => 0
    case x: Right[_] => 1
  }

  implicit def mirror[L, R]: Mirror.Sum {
    type _MonoType = Either[L, R]
    type ElemTypes = (Left[L], Right[R])
  } = this.asInstanceOf

  implicit def derived$Eq[L: Eq, R: Eq]: Eq[Either[L, R]] = Eq.derived
  implicit def derived$Pickler[L: Pickler, R: Pickler]: Pickler[Either[L, R]] = Pickler.derived
  implicit def derived$Show[L: Show, R: Show]: Show[Either[L, R]] = Show.derived
}

case class Left[L](elem: L) extends Either[L, Nothing]
case class Right[R](elem: R) extends Either[Nothing, R]

object Left extends Mirror.Product {
  type _MonoType = Left[_]
  def _fromProduct(p: Product): Left[_] = Left(productElement[Any](p, 0))
  implicit def mirror[L]: Mirror.Product {
    type _MonoType = Left[L]
    type ElemTypes = L *: Unit
    type CaseLabel = "Left"
    type ElemLabels = "x" *: Unit
  } = this.asInstanceOf
}

object Right extends Mirror.Product {
  type _MonoType = Right[_]
  def _fromProduct(p: Product): Right[_] = Right(productElement[Any](p, 0))
  implicit def mirror[R]: Mirror.Product {
    type _MonoType = Right[R]
    type ElemTypes = R *: Unit
    type CaseLabel = "Right"
    type ElemLabels = "x" *: Unit
  } = this.asInstanceOf
}

// --------------- Equality typeclass ---------------------------------

trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.erasedValue

  inline def tryEql[T](x: T, y: T) = implied match {
    case eq: Eq[T] => eq.eql(x, y)
  }

  inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryEql[elem](productElement[elem](x, n), productElement[elem](y, n)) &&
        eqlElems[elems1](n + 1)(x, y)
      case _: Unit =>
        true
    }

  inline def eqlProduct[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
    eqlElems[m.ElemTypes](0)(x, y)

  inline def eqlCases[Alts](n: Int)(x: Any, y: Any, ord: Int): Boolean =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          implied match {
            case m: Mirror.ProductOf[`alt`] => eqlElems[m.ElemTypes](0)(x, y)
          }
        else eqlCases[alts1](n + 1)(x, y, ord)
      case _: Unit =>
        false
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): Eq[T] = new Eq[T] {
    def eql(x: T, y: T): Boolean =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = m.ordinal(x)
          ord == m.ordinal(y) && eqlCases[m.ElemTypes](0)(x, y, ord)
        case m: Mirror.ProductOf[T] =>
          eqlElems[m.ElemTypes](0)(x, y)
      }
  }

  implicit object IntEq extends Eq[Int] {
    def eql(x: Int, y: Int) = x == y
  }
}

// ----------- Another typeclass -----------------------------------

trait Pickler[T] {
  def pickle(buf: mutable.ListBuffer[Int], x: T): Unit
  def unpickle(buf: mutable.ListBuffer[Int]): T
}

object Pickler {
  import scala.compiletime.{erasedValue, constValue}

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = implied match {
    case pkl: Pickler[T] => pkl.pickle(buf, x)
  }

  inline def pickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        tryPickle[elem](buf, productElement[elem](x, n))
        pickleElems[elems1](n + 1)(buf, x)
      case _: Unit =>
    }

  inline def pickleCases[Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any, ord: Int): Unit =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          implied match {
            case m: Mirror.ProductOf[`alt`] => pickleElems[m.ElemTypes](0)(buf, x)
          }
        else pickleCases[alts1](n + 1)(buf, x, ord)
      case _: Unit =>
    }

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = implied match {
    case pkl: Pickler[T] => pkl.unpickle(buf)
  }

  inline def unpickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], elems: ArrayProduct): Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        elems(n) = tryUnpickle[elem](buf).asInstanceOf[AnyRef]
        unpickleElems[elems1](n + 1)(buf, elems)
      case _: Unit =>
    }

  inline def unpickleCase[T, Elems <: Tuple](buf: mutable.ListBuffer[Int], m: Mirror.ProductOf[T]): T = {
    inline val size = constValue[Tuple.Size[Elems]]
    inline if (size == 0)
      m._fromProduct(EmptyProduct)
    else {
      val elems = new ArrayProduct(size)
      unpickleElems[Elems](0)(buf, elems)
      m._fromProduct(elems)
    }
  }

  inline def unpickleCases[T, Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], ord: Int): T =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          implied match {
            case m: Mirror.ProductOf[`alt` & T] =>
              unpickleCase[`alt` & T, m.ElemTypes](buf, m)
          }
        else unpickleCases[T, alts1](n + 1)(buf, ord)
      case _: Unit =>
        throw new IndexOutOfBoundsException(s"unexpected ordinal number: $ord")
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): Pickler[T] = new {
    def pickle(buf: mutable.ListBuffer[Int], x: T): Unit =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = m.ordinal(x)
          buf += ord
          pickleCases[m.ElemTypes](0)(buf, x, ord)
        case m: Mirror.ProductOf[T] =>
          pickleElems[m.ElemTypes](0)(buf, x)
      }
    def unpickle(buf: mutable.ListBuffer[Int]): T =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = nextInt(buf)
          unpickleCases[T, m.ElemTypes](0)(buf, ord)
        case m: Mirror.ProductOf[T] =>
          unpickleCase[T, m.ElemTypes](buf, m)
      }
  }

  implicit object IntPickler extends Pickler[Int] {
    def pickle(buf: mutable.ListBuffer[Int], x: Int): Unit = buf += x
    def unpickle(buf: mutable.ListBuffer[Int]): Int = nextInt(buf)
  }
}

// ----------- A third typeclass, making use of labels --------------------------

trait Show[T] {
  def show(x: T): String
}
object Show {
  import scala.compiletime.{erasedValue, constValue}

  inline def tryShow[T](x: T): String = implied match {
    case s: Show[T] => s.show(x)
  }

  inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Any): List[String] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val formal = constValue[label]
            val actual = tryShow(productElement[elem](x, n))
            s"$formal = $actual" :: showElems[elems1, labels1](n + 1)(x)
        }
      case _: Unit =>
        Nil
  }

  inline def showCase(x: Any, m: Mirror.ProductOf[_]): String = {
    val label = constValue[m.CaseLabel]
    inline m match {
      case m: Mirror.Singleton => label
      case _ => showElems[m.ElemTypes, m.ElemLabels](0)(x).mkString(s"$label(", ", ", ")")
    }
  }

  inline def showCases[Alts <: Tuple](n: Int)(x: Any, ord: Int): String =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          implied match {
            case m: Mirror.ProductOf[`alt`] =>
              showCase(x, m)
          }
        else showCases[alts1](n + 1)(x, ord)
      case _: Unit =>
        throw new MatchError(x)
    }

  inline def derived[T](implicit ev: Mirror.Of[T]): Show[T] = new {
    def show(x: T): String =
      inline ev match {
        case m: Mirror.SumOf[T] =>
          val ord = m.ordinal(x)
          showCases[m.ElemTypes](0)(x, ord)
        case m: Mirror.ProductOf[T] =>
          showCase(x, m)
      }
  }

  implicit object IntShow extends Show[Int] {
    def show(x: Int): String = x.toString
  }
}

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