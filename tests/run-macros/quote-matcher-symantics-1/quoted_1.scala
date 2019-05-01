
import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection


object Macros {

  inline def lift[T](sym: Symantics[T])(a: => DSL): T = ${impl[T]('sym, 'a)}

  private def impl[T: Type](sym: Expr[Symantics[T]], a: Expr[DSL])(implicit reflect: Reflection): Expr[T] = {

    def lift(e: Expr[DSL]): Expr[T] = e match {

      case '{ LitDSL(${ Const(c) }) } =>
      // case scala.internal.quoted.Matcher.unapply[Tuple1[Expr[Int]]](Tuple1(Literal(c)))(/*implicits*/ '{ LitDSL(patternHole[Int]) }, reflect) =>
        '{ $sym.value(${c.toExpr}) }

      case '{ ($x: DSL) + ($y: DSL) } =>
      // case scala.internal.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple2(x, y))(/*implicits*/ '{ patternHole[DSL] + patternHole[DSL] }, reflect) =>
        '{ $sym.plus(${lift(x)}, ${lift(y)}) }

      case '{ ($x: DSL) * ($y: DSL) } =>
       // case scala.internal.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple2(x, y))(/*implicits*/ '{ patternHole[DSL] * patternHole[DSL] }, reflect) =>
        '{ $sym.times(${lift(x)}, ${lift(y)}) }

      case _ =>
        import reflect._
        error("Expected explicit DSL", e.unseal.pos)
        '{ ??? }

    }

    lift(a)
  }

}

trait DSL {
  def + (x: DSL): DSL = ???
  def * (x: DSL): DSL = ???
}
case class LitDSL(x: Int) extends DSL

trait Symantics[Num] {
  def value(x: Int): Num
  def plus(x: Num, y: Num): Num
  def times(x: Num, y: Num): Num
}
