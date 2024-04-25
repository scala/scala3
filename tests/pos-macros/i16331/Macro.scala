import scala.quoted._
object Macro:
  transparent inline def macroDef[A](): Int = ${ macroDefImpl() }
  def macroDefImpl()(using q: Quotes): Expr[Int] = '{0}
