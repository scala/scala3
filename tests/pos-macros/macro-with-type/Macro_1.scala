import scala.quoted._
object Macro {
  inline def ff: Unit = ${impl('[Int])}
  def impl(t: TypeTag[Int])(given QuoteContext): Expr[Unit] = '{}
}
