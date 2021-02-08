import scala.quoted.*

case class T(x: Type[_ <: Any])

object T {
  def impl[A](t: T)(using ctx: Quotes): Expr[Unit] = {
    Expr.summon[t.x.Underlying] // error // error
    '{}
  }
}