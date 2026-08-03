package sangria

import scala.quoted.*

object Macros:

  transparent inline def useOrNullFromTransparentInline[T](inline o: Option[T]): T =
    o.orNull.asInstanceOf[T]

  inline def useOrNullFromInline[T](inline o: Option[T]): T =
    o.orNull.asInstanceOf[T]

  inline def useOrNullFromMacro[T](inline o: Option[T]): T =
    ${ useOrNullImpl('o) }

  def useOrNullImpl[T: Type](o: Expr[Option[T]])(using Quotes): Expr[T] =
    '{ $o.orNull.asInstanceOf[T] }
