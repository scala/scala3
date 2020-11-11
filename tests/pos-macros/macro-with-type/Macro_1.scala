import scala.quoted._
object Macro {
  inline def ff: Unit = ${impl(Type.of[Int])}
  def impl(t: Type[Int])(using QuoteContext): Expr[Unit] = '{}
}
