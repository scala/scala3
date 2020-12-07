import scala.quoted._

object Macros {

  inline def dealias[T]: String = ${ impl[T] }

  def impl[T: Type](using Quotes) : Expr[String] = {
    import quotes.reflect._
    Value(TypeRepr.of[T].dealias.show)
  }
}
