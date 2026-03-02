import scala.quoted.*

opaque type Box[A] = Any
object Box:
  transparent inline def pack[A]: Nothing => Box[A] = ${ packImpl[A] }

  private def packImpl[A](using Quotes, Type[A]): Expr[Nothing => Box[A]] =
    import quotes.reflect.*
    report.errorAndAbort("Not implemented")
