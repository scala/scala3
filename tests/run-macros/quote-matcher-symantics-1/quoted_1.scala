
import scala.quoted._
import scala.quoted.matching._

object Macros {

  inline def lift[T](sym: Symantics[T])(a: => DSL): T = ${impl[T]('sym, 'a)}

  private def impl[T: Type](sym: Expr[Symantics[T]], a: Expr[DSL]) given (qctx: QuoteContext): Expr[T] = {

    def lift(e: Expr[DSL]): Expr[T] = e match {

      case '{ LitDSL(${ Const(c) }) } =>
        '{ $sym.value(${c.toExpr}) }

      case '{ ($x: DSL) + ($y: DSL) } =>
        '{ $sym.plus(${lift(x)}, ${lift(y)}) }

      case '{ ($x: DSL) * ($y: DSL) } =>
        '{ $sym.times(${lift(x)}, ${lift(y)}) }

      case _ =>
        import qctx.tasty._
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
