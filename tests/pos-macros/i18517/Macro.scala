package dummy

import scala.quoted.*

object Macro:
  def impl()(using quotes:Quotes) : Expr[Any] =
    '{ null }
