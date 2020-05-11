package foo

case class FirstArg(value: Any, source: String)
object FirstArg {
  inline given create as FirstArg = ${Macros.argsImpl}
}

object Macros {
  import scala.quoted._

  def argsImpl(using s: Scope): s.Expr[FirstArg] = {
    import s.tasty._

    def enclosingClass(cur: Symbol = Symbol.currentOwner): Symbol =
      if (cur.isClassDef) cur
      else enclosingClass(cur.owner)

    def enclosingParamList(owner: Symbol): Seq[Seq[Symbol]] =
      if owner.isClassDef then
        owner.tree match
          case tdef: ClassDef =>
            tdef.constructor.paramss map { _ map {_.symbol }}
      else enclosingParamList(owner.owner)

    val paramss = enclosingParamList(Symbol.currentOwner)
    val firstArg = paramss.flatten.head
    val ref = Select.unique(This(enclosingClass()), firstArg.name)
    '{ FirstArg(${ref.seal}, ${Expr(firstArg.name)}) }
  }
}
