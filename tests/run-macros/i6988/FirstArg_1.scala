package foo

case class FirstArg(value: Any, source: String)
object FirstArg {
  inline given create: FirstArg = ${Macros.argsImpl}
}

object Macros {
  import scala.quoted._

  def argsImpl(given qctx: QuoteContext): Expr[FirstArg] = {
    import qctx.tasty.{_, given}

    var enclosingCls: Symbol = rootContext.owner

    def enclosingParamList(owner: Symbol): Seq[Seq[Symbol]] =
      if owner.isClassDef then
        owner.tree match
          case tdef: ClassDef =>
            tdef.constructor.paramss map { _ map {_.symbol }}
      else enclosingParamList(owner.owner)

    def literal(value: String): Expr[String] =
      Literal(Constant(value)).seal.asInstanceOf[Expr[String]]
    val paramss = enclosingParamList(rootContext.owner)
    val firstArg = paramss.flatten.head
    val ref = Select.unique(This(enclosingCls), firstArg.name)
    '{ FirstArg(${ref.seal}, ${Expr(firstArg.name)}) }
  }
}
