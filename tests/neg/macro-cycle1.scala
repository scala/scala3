import scala.quoted.{Expr, QuoteContext}
object Test {
  def fooImpl(using QuoteContext): Expr[Unit] = '{println("hi")}

  inline def foo: Unit = ${fooImpl}

  foo // error
}
