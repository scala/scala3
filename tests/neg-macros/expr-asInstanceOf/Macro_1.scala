import scala.quoted.*

object Macro:
  inline def foo(): Int =
    ${fooImpl()}

  def fooImpl()(using Quotes): Expr[Int] =
    import quotes.reflect.*
    Expr("hello").asInstanceOf[Expr[Int]]
