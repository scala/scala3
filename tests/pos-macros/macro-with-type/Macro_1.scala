import scala.quoted.{_, given}
object Macro {
  inline def ff: Unit = ${impl('[Int])}
  def impl(t: Type[Int])(given QuoteContext): Expr[Unit] = '{}
}
