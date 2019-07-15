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


// --------------- Equality typeclass ---------------------------------

trait Eq[T] {
  def eql(x: T, y: T): Boolean
}

object Eq {
  import scala.compiletime.{erasedValue, summonFrom}

  inline def tryEql[T](x: T, y: T) = summonFrom {
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
          summonFrom {
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
  import scala.compiletime.{erasedValue, constValue, summonFrom}

  def nextInt(buf: mutable.ListBuffer[Int]): Int = try buf.head finally buf.trimStart(1)

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit = summonFrom {
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
          summonFrom {
            case m: Mirror.ProductOf[`alt`] => pickleElems[m.ElemTypes](0)(buf, x)
          }
        else pickleCases[alts1](n + 1)(buf, x, ord)
      case _: Unit =>
    }

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T = summonFrom {
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
          summonFrom {
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
  import scala.compiletime.{erasedValue, constValue, summonFrom}

  inline def tryShow[T](x: T): String = summonFrom {
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
          summonFrom {
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
