import scala.quoted._

object Macro {
  transparent inline def transform[T](inline expr: T): T = ${ transformImpl[T]('expr) }
  def transformImpl[T: Type](f: Expr[T])(using Quotes): Expr[T] = f
}
