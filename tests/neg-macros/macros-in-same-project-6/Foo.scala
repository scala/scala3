import scala.quoted._

object Foo {

  inline def myMacro(): Unit = ${ aMacroImplementation }

  def aMacroImplementation(using s: Scope): s.Expr[Unit] = {
    import s.tasty._
    error("some error", rootPosition)
    throw new NoClassDefFoundError("Bar$")
  }
}
