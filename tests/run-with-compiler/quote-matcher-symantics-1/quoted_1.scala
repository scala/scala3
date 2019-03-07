
import scala.quoted._
import scala.quoted.matching._

import scala.tasty.Reflection

import scala.runtime.quoted.Matcher.Hole
import scala.runtime.quoted.Matcher.hole

object Macros {

  inline def lift[T](sym: Symantics[T])(a: => DSL): T = ${impl[T]('sym, 'a)}

  private def impl[T: Type](sym: Expr[Symantics[T]], a: Expr[DSL])(implicit reflect: Reflection): Expr[T] = {

    object ValueExpr extends ExprMatch[Tuple1[Expr[Int]]]('{LitDSL(hole[Int])}) // case '{ LitDSL($x) } =>
    object PlusExpr extends ExprMatch[Tuple2[Expr[DSL], Expr[DSL]]]('{hole[DSL] + hole[DSL]}) // case '{ ($x: DSL) + ($y: DSL) } =>
    object TimesExpr extends ExprMatch[Tuple2[Expr[DSL], Expr[DSL]]]('{hole[DSL] * hole[DSL]}) // case '{ ($x: DSL) * ($y: DSL) } =>

    def lift(e: Expr[DSL]): Expr[T] = e match {

      // case '{ LitDSL(${Literal(c)}) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple1(Literal(c)))(/*implicits*/ '{ LitDSL(hole) }, reflect) =>
      case ValueExpr(Tuple1(Literal(c: Int))) =>
        '{ $sym.value(${c.toExpr}) }

      // case '{ ($x: DSL) + ($y: DSL) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple2(x, y))(/*implicits*/ '{ (hole: DSL) + (hole: DSL) }, reflect) =>
      case PlusExpr(Tuple2(x, y)) =>
        '{ $sym.plus(${lift(x)}, ${lift(y)}) }

      // case '{ ($x: DSL) * ($y: DSL) } =>
      // case scala.runtime.quoted.Matcher.unapply[Tuple2[Expr[DSL], Expr[DSL]]](Tuple(x, y))(/*implicits*/ '{ (hole: DSL) * (hole: DSL) }, reflect) =>
      case TimesExpr(Tuple2(x, y)) =>
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

class ExprMatch[Tup <: Tuple](pattern: Expr[_]) {
  def unapply(x: Expr[_])(implicit reflect: Reflection): Option[Tup] =
    scala.runtime.quoted.Matcher.unapply(x)(pattern, reflect)
}
