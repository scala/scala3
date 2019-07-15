import scala.collection.mutable
import scala.annotation.tailrec

import scala.quoted._

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

  inline def tryEql[T](x: T, y: T) =
    ${ tryEqlExpr('x, 'y) }

  private def tryEqlExpr[T: Type](x: Expr[T], y: Expr[T])(using Quotes): Expr[Boolean] = {
    Expr.summon[Eq[T]] match {
      case Some(eq) => '{ $eq.eql($x, $y) }
      case None =>
        quotes.reflect.report.error(s"Could not find delegate for Eq[${Type.show[T]}]")
        '{ false }
    }
  }

  inline def eqlElems[Elems <: Tuple](n: Int)(x: Any, y: Any): Boolean =
    ${ eqlElemsExpr[Elems]('n)('x, 'y) }

  private def eqlElemsExpr[Elems <: Tuple](n: Expr[Int])(x: Expr[Any], y: Expr[Any])(using Type[Elems])(using Quotes): Expr[Boolean] =
    '{ ??? : Elems } match {
      case '{ type elems1 <: Tuple; $_ : (elem *: `elems1`) } =>
        '{
          ${ tryEqlExpr[elem]('{ productElement[elem]($x, $n) }, '{ productElement[elem]($y, $n) }) } &&
          ${ eqlElemsExpr[elems1]('{ $n + 1 })(x, y) }
        }
      case '{ $_ : EmptyTuple } =>
        '{true}
    }

  inline def eqlProduct[T](m: Mirror.ProductOf[T])(x: Any, y: Any): Boolean =
    eqlElems[m.ElemTypes](0)(x, y)

  inline def eqlCases[Alts](inline n: Int)(x: Any, y: Any, ord: Int): Boolean =
    ${ eqlCasesExpr[Alts]('n)('x, 'y, 'ord) }

  def eqlCasesExpr[Alts](n: Expr[Int])(x: Expr[Any], y: Expr[Any], ord: Expr[Int])(using Type[Alts])(using Quotes): Expr[Boolean] =
    '{ ??? : Alts } match {
      case '{ $_ : (alt *: alts1) } =>
        '{
          if ($ord == $n)
            ${
              Expr.summon[Mirror.ProductOf[alt]] match {
                case Some('{ $mm: tt }) =>
                  '{
                    val m = $mm
                    type ET = m.ElemTypes
                    ${ eqlElemsExpr[ET](Expr(0))(x, y) }
                  }
                case _ =>
                  quotes.reflect.report.errorAndAbort(s"Could not find given for ProductOf[${Type.show[alt]}]")
              }
            }
          else ${ eqlCasesExpr[alts1](Expr(n.valueOrAbort + 1))(x, y, ord) }
        }
      case '{ $x: EmptyTuple } =>
        '{ false }
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

  inline def tryPickle[T](buf: mutable.ListBuffer[Int], x: T): Unit =
    ${ tryPickleExpr('buf, 'x) }

  private def tryPickleExpr[T: Type](buf: Expr[mutable.ListBuffer[Int]], x: Expr[T])(using Quotes): Expr[Unit] = {
    Expr.summon[Pickler[T]] match {
      case Some(pkl) => '{ $pkl.pickle($buf, $x) }
      case None =>
        quotes.reflect.report.error(s"Could not find delegate for Pickler[${Type.show[T]}]")
        '{}
    }
  }

  inline def pickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any): Unit =
    ${ pickleElemsExpr[Elems]('n)('buf, 'x) }

  def pickleElemsExpr[Elems <: Tuple : Type](n: Expr[Int])(buf: Expr[mutable.ListBuffer[Int]], x: Expr[Any])(using Quotes): Expr[Unit] =
    '{ ??? : Elems } match {
      case '{ type elems1 <: Tuple; $_ : (elem *: `elems1`) } =>
        '{
          ${ tryPickleExpr[elem](buf,  '{ productElement[elem]($x, $n) }) }
          ${ pickleElemsExpr[elems1]('{$n + 1})(buf, x) }
        }
      case '{ $_ : EmptyTuple } =>
        '{}
    }

  inline def pickleCases[Alts <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], x: Any, ord: Int): Unit =
    inline erasedValue[Alts] match {
      case _: (alt *: alts1) =>
        if (ord == n)
          summonFrom {
            case m: Mirror.ProductOf[`alt`] => pickleElems[m.ElemTypes](0)(buf, x)
          }
        else pickleCases[alts1](n + 1)(buf, x, ord)
      case _: EmptyTuple =>
    }

  inline def tryUnpickle[T](buf: mutable.ListBuffer[Int]): T =
    ${ tryUnpickleExpr[T]('buf) }

  private def tryUnpickleExpr[T: Type](buf: Expr[mutable.ListBuffer[Int]])(using Quotes): Expr[T] = {
    Expr.summon[Pickler[T]] match {
      case Some(pkl) => '{ $pkl.unpickle($buf) }
      case None =>
        quotes.reflect.report.error(s"Could not find delegate for Pickler[${Type.show[T]}]")
        '{ ??? }
    }
  }

  inline def unpickleElems[Elems <: Tuple](n: Int)(buf: mutable.ListBuffer[Int], elems: ArrayProduct): Unit =
    ${ unpickleElemsExpr[Elems]('n)('buf, 'elems) }

  private def unpickleElemsExpr[Elems <: Tuple : Type](n: Expr[Int])(buf: Expr[mutable.ListBuffer[Int]], elems: Expr[ArrayProduct])(using Quotes): Expr[Unit] =
    '{ ??? : Elems } match {
      case '{ type elems1 <: Tuple; $_ : (elem *: `elems1`) } =>
        '{
          $elems.update($n, ${ tryUnpickleExpr[elem](buf) }.asInstanceOf[AnyRef])
          ${ unpickleElemsExpr[elems1]('{$n + 1})(buf, elems) }
        }
      case '{ $_ : EmptyTuple } =>
        '{}
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
      case _: EmptyTuple =>
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

  inline def tryShow[T](x: T): String =
    ${ tryShowExpr('x) }

  private def tryShowExpr[T: Type](x: Expr[T])(using Quotes): Expr[String] = {
    Expr.summon[Show[T]] match {
      case Some(s) => '{ $s.show($x) }
      case None =>
        quotes.reflect.report.error(s"Could not find delegate for Show[${Type.show[T]}]")
        '{ "" }
    }
  }

  inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Any): List[String] =
    ${ unpickleElemsExpr[Elems, Labels]('n)('x) }

  private def unpickleElemsExpr[Elems <: Tuple : Type, Labels <: Tuple : Type](n: Expr[Int])(x: Expr[Any])(using Quotes): Expr[List[String]] =
    '{ ??? : Elems } match {
      case '{ type elems1 <: Tuple; $_ : (elem *: `elems1`) } =>
        '{ ??? : Labels } match {
          case '{ type label <: String; type labels1 <: Tuple; $_ : (`label` *: `labels1`) } =>
            val label0 = Expr(Type.valueOfConstant[label].get)
            '{
              val formal = $label0
              val actual = ${ tryShowExpr('{productElement[elem]($x, $n)}) }
              s"$formal = $actual" :: ${ unpickleElemsExpr[elems1, labels1]('{$n + 1})(x) }
            }
        }
      case '{ $_ : EmptyTuple } =>
        '{ Nil }
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
      case _: EmptyTuple =>
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
