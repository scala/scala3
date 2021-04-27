import scala.quoted.*

object Foo {
  inline def foo[T <: AnyKind]: String = ${ bar[T] }

  def bar[T <: AnyKind : Type](using Quotes): Expr[String] = {
    import quotes.reflect.*

    def packageToName(sym: Symbol): Unit = {
      if sym.isPackageDef then
        packageToName(sym.owner)
    }

    val sym = TypeRepr.of[T].typeSymbol
    if (!sym.isNoSymbol) {
      sym.tree match {
        case c: ClassDef =>
          if (!sym.maybeOwner.isNoSymbol) {
            if sym.maybeOwner.isPackageDef then
              packageToName(sym.maybeOwner)
          }
      }
    }
    Expr("")
  }
}
