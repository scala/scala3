import scala.quoted.*
object Macro {
  inline def ff[T](implicit t: Type[T]): Int = ${ impl[T] }
  def impl[T](using Quotes): Expr[Int] = '{4}
}
