import scala.quoted._


object Foo {

  inline def inspectBody(inline i: Int): String =
    ${ inspectBodyImpl('i) }

  def inspectBodyImpl(using s: Scope)(x: s.Expr[Int]): s.Expr[String] = {
    import s.tasty._

    def definitionString(sym: Symbol): s.Expr[String] =
      if sym.isClassDef || sym.isDefDef || sym.isValDef then Expr(sym.tree.showExtractors)
      else '{"NO DEFINTION"}

    x match {
      case Inlined(None, Nil, arg) => definitionString(arg.symbol)
      case arg => definitionString(arg.symbol) // TODO should all by name parameters be in an inline node
    }
  }

  def foo: Int = 1 + 2
  val bar: Int = 2 + 3
}
