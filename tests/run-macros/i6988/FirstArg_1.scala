package foo

case class FirstArg(value: Any, source: String)
object FirstArg {
  inline given create: FirstArg = ${Macros.argsImpl}
}

object Macros {
  import scala.quoted.*

  def argsImpl(using Quotes) : Expr[FirstArg] = {
    import quotes.reflect.*

    def enclosingClass(cur: Symbol = Symbol.spliceOwner): Symbol =
      if (cur.isClassDef) cur
      else enclosingClass(cur.owner)

    def enclosingParamList(owner: Symbol): Seq[Seq[Symbol]] =
      if owner.isClassDef then
        owner.tree match
          case tdef: ClassDef =>
            tdef.constructor.paramss map { _.params map {_.symbol }}
      else enclosingParamList(owner.owner)

    def literal(value: String): Expr[String] =
      Literal(StringConstant(value)).asExpr.asInstanceOf[Expr[String]]
    val paramss = enclosingParamList(Symbol.spliceOwner)
    val firstArg = paramss.flatten.head
    val ref = Select.unique(This(enclosingClass()), firstArg.name)
    '{ FirstArg(${ref.asExpr}, ${Expr(firstArg.name)}) }
  }
}
