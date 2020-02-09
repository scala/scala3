import scala.quoted._
object Macro {
  inline def ff[T](implicit t: Type[T]): Int = ${ impl[T] }
  def impl[T](using QuoteContext): Expr[Int] = '{4}
}
