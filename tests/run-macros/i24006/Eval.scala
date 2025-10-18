
import scala.quoted._

object Eval:

  inline def eval[T](x: List[T]) = ${ evalImpl[T]('x) }
  def evalImpl[A](x: Expr[List[A]])(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    println(x.asTerm.tpe.widen)
    x.asTerm.tpe.widen.typeArgs.head
    '{()}
