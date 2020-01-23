import scala.quoted.{Expr, QuoteContext}
object Test {
  def fooImpl with QuoteContext : Expr[Unit] = '{println("hi")}

  inline def foo: Unit = ${fooImpl}

  foo // error
}
