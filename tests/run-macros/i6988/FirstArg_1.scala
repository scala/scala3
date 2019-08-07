package foo

case class FirstArg(value: Any, source: String)
object FirstArg {
  inline given as FirstArg = ${Macros.argsImpl}
}

object Macros {
  import language.experimental.macros
  import scala.quoted._
  import scala.tasty._

  def argsImpl given (qctx: QuoteContext): Expr[FirstArg] = {
    import qctx.tasty._
    var enclosingCls: ClassDefSymbol = rootContext.owner.asInstanceOf[ClassDefSymbol]
    def enclosingParamList(owner: Symbol): Seq[Seq[Symbol]] =
      owner match {
        case IsClassDefSymbol(x) =>
          enclosingCls = x
          x.tree.constructor.paramss map { _ map {
            _.symbol
          }}
        case _ =>
          enclosingParamList(owner.owner)
      }
    def literal(value: String): Expr[String] =
      Literal(Constant(value)).seal.asInstanceOf[Expr[String]]
    val paramss = enclosingParamList(rootContext.owner)
    val firstArg = paramss.flatten.head
    val ref = Select.unique(This(enclosingCls), firstArg.name)
    '{ FirstArg(${ref.seal}, ${firstArg.name.toExpr}) }
  }
}