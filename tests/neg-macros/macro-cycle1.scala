import scala.quoted.{Expr, Quotes}
object Test {
  def fooImpl(using Quotes): Expr[Unit] = '{println("hi")}

  inline def foo: Unit = ${fooImpl}

  foo // error
}
