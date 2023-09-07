import scala.quoted.*

object T {
  def impl[A](t: Type[A])(using Quotes): Expr[Unit] = {
    Expr.summon[t.Underlying] // error
    '{}
  }
}