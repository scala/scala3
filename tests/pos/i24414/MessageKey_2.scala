import scala.quoted.*

final class MessageKey

object MessageKey:
  inline given Constraint[String, MessageKey] with
    override inline def test: Boolean = ${ check() }

  def check()(using Quotes): Expr[Boolean] = Expr(true)
