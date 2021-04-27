import scala.quoted.*

object T {
  def impl[A](using t: Type[A])(using Quotes): Expr[Unit] = {
    Expr.summon[t.Underlying] // error
    '{}
  }
}