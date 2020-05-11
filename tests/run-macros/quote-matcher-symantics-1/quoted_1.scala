
import scala.quoted._


object Macros {

  inline def lift[T](sym: Symantics[T])(inline a: DSL): T = ${impl[T]('sym, 'a)}

  private def impl[T](using s: Scope)(sym: s.Expr[Symantics[T]], a: s.Expr[DSL])(using s.Type[T]): s.Expr[T] = {

    def lift(e: s.Expr[DSL]): s.Expr[T] = e match {

      case '{ LitDSL(${ Const(c) }) } =>
        '{ $sym.value(${Expr(c)}) }

      case '{ ($x: DSL) + ($y: DSL) } =>
        '{ $sym.plus(${lift(x)}, ${lift(y)}) }

      case '{ ($x: DSL) * ($y: DSL) } =>
        '{ $sym.times(${lift(x)}, ${lift(y)}) }

      case _ =>
        import s.tasty._
        error("Expected explicit DSL", e.pos)
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
