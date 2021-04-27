
import scala.quoted.*


object Macros {

  inline def lift[T](sym: Symantics[T])(inline a: DSL): T = ${impl[T]('sym, 'a)}

  private def impl[T: Type](sym: Expr[Symantics[T]], a: Expr[DSL])(using Quotes): Expr[T] = {

    def lift(e: Expr[DSL]): Expr[T] = e match {

      case '{ LitDSL(${ Expr(c) }) } =>
        '{ $sym.value(${Expr(c)}) }

      case '{ ($x: DSL) + ($y: DSL) } =>
        '{ $sym.plus(${lift(x)}, ${lift(y)}) }

      case '{ ($x: DSL) * ($y: DSL) } =>
        '{ $sym.times(${lift(x)}, ${lift(y)}) }

      case _ =>
        import quotes.reflect.*
        report.error("Expected explicit DSL", e.asTerm.pos)
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
