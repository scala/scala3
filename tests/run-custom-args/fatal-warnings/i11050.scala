import scala.compiletime.*
import scala.deriving.*

object OriginalReport:
  sealed trait TreeValue
  sealed trait SubLevel extends TreeValue
  case class Leaf1(value: String) extends TreeValue
  case class Leaf2(value: Int)    extends SubLevel

// Variants from the initial failure in akka.event.LogEvent
object FromAkkaCB:
  sealed trait A
  sealed trait B extends A
  sealed trait C extends A
  case class D() extends B, C
  case class E() extends C, B

object FromAkkaCB2:
  sealed trait A
  sealed trait N extends A
  case class B() extends A
  case class C() extends A, N

object FromAkkaCB3:
  sealed trait A
  case class B() extends A
  case class C() extends A
  class D extends C // ignored pattern: class extending a case class

object NoUnreachableWarnings:
  sealed trait Top
  object Top

  final case class MiddleA() extends Top with Bottom
  final case class MiddleB() extends Top with Bottom
  final case class MiddleC() extends Top with Bottom

  sealed trait Bottom extends Top

object FromAkkaCB4:
  sealed trait LogEvent
  object LogEvent
  case class Error() extends LogEvent
  class Error2() extends Error() with LogEventWithMarker // ignored pattern
  case class Warning() extends LogEvent
  sealed trait LogEventWithMarker extends LogEvent // must be defined late

object FromAkkaCB4simpler:
  sealed trait LogEvent
  object LogEvent
  case class Error() extends LogEvent
  class Error2() extends LogEventWithMarker // not a case class
  case class Warning() extends LogEvent
  sealed trait LogEventWithMarker extends LogEvent

object Test:
  def main(args: Array[String]): Unit =
    testOriginalReport()
    testFromAkkaCB()
    testFromAkkaCB2()
  end main

  def testOriginalReport() =
    import OriginalReport._
    val m                 = summon[Mirror.SumOf[TreeValue]]
    given Show[TreeValue] = Show.derived[TreeValue]
    val leaf1             = Leaf1("1")
    val leaf2             = Leaf2(2)

    assertEq(List(leaf1, leaf2).map(m.ordinal), List(1, 0))
    assertShow[TreeValue](leaf1, "[1] Leaf1(value = \"1\")")
    assertShow[TreeValue](leaf2, "[0] [0] Leaf2(value = 2)")
  end testOriginalReport

  def testFromAkkaCB() =
    import FromAkkaCB._
    val m         = summon[Mirror.SumOf[A]]
    given Show[A] = Show.derived[A]
    val d         = D()
    val e         = E()

    assertEq(List(d, e).map(m.ordinal), List(0, 0))
    assertShow[A](d, "[0] [0] D")
    assertShow[A](e, "[0] [1] E")
  end testFromAkkaCB

  def testFromAkkaCB2() =
    import FromAkkaCB2._
    val m         = summon[Mirror.SumOf[A]]
    val n         = summon[Mirror.SumOf[N]]
    given Show[A] = Show.derived[A]
    val b         = B()
    val c         = C()

    assertEq(List(b, c).map(m.ordinal), List(1, 0))
    assertShow[A](b, "[1] B")
    assertShow[A](c, "[0] [0] C")
  end testFromAkkaCB2

  def assertEq[A](obt: A, exp: A)          = assert(obt == exp, s"$obt != $exp (obtained != expected)")
  def assertShow[A: Show](x: A, s: String) = assertEq(Show.show(x), s)
end Test

trait Show[-T]:
  def show(x: T): String

object Show:
  given Show[Int]    with { def show(x: Int)    = s"$x"     }
  given Show[Char]   with { def show(x: Char)   = s"'$x'"   }
  given Show[String] with { def show(x: String) = s"$"$x$"" }

  inline def show[T](x: T): String = summonInline[Show[T]].show(x)

  transparent inline def derived[T](implicit ev: Mirror.Of[T]): Show[T] = new {
    def show(x: T): String = inline ev match {
      case m: Mirror.ProductOf[T] => showProduct(x.asInstanceOf[Product], m)
      case m: Mirror.SumOf[T]     => showCases[m.MirroredElemTypes](0)(x, m.ordinal(x))
    }
  }

  transparent inline def showProduct[T](x: Product, m: Mirror.ProductOf[T]): String =
    constValue[m.MirroredLabel] + showElems[m.MirroredElemTypes, m.MirroredElemLabels](0, Nil)(x)

  transparent inline def showElems[Elems <: Tuple, Labels <: Tuple](n: Int, elems: List[String])(x: Product): String =
    inline (erasedValue[Labels], erasedValue[Elems]) match {
      case _: (label *: labels, elem *: elems) =>
        val value = show(x.productElement(n).asInstanceOf[elem])
        showElems[elems, labels](n + 1, s"${constValue[label]} = $value" :: elems)(x)
      case _: (EmptyTuple, EmptyTuple)         =>
        if elems.isEmpty then "" else elems.mkString(s"(", ", ", ")")
    }

  transparent inline def showCases[Alts <: Tuple](n: Int)(x: Any, ord: Int): String =
    inline erasedValue[Alts] match {
      case _: (alt *: alts) =>
        if (ord == n) summonFrom {
          case m: Mirror.Of[`alt`] => s"[$ord] " + derived[alt](using m).show(x.asInstanceOf[alt])
        } else showCases[alts](n + 1)(x, ord)
      case _: EmptyTuple => throw new MatchError(x)
    }
end Show
