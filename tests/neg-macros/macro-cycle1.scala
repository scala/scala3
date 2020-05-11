import scala.quoted.Scope
object Test {
  def fooImpl(using s: Scope): s.Expr[Unit] = '{println("hi")}

  inline def foo: Unit = ${fooImpl}

  foo // error
}
