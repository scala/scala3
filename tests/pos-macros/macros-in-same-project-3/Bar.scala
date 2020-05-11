import scala.quoted._

object Bar {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using s: Scope): s.Expr[Unit] = Foo.hello()

}
