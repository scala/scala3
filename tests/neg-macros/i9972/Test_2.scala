package notmacro

import scala.quoted.*

case class T[A <: AnyKind](s: String)

object T {
  implicit inline def derived[A <: AnyKind]: T[A] = ${ reprImpl[A] }

  def reprImpl[A <: AnyKind](using t: Type[A])(using ctx: Quotes): Expr[T[A]] =
    '{ T[A]("") }
}
