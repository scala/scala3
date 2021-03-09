import quoted.*

inline def printInfos(inline x: Any): Unit = ${ infoExpr('x) }

private def infoExpr(x: Expr[Any])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*
  val sb = StringBuilder()
  new TreeTraverser {
    override def traverseTree(tree: Tree)(owner: Symbol): Unit =
      tree match
        case tree: Definition =>
          sb.append(
            s"""${tree.name}:
               |  ${tree.symbol.info.show}
               |  ${tree.symbol.info.show(using Printer.TypeReprStructure)}
               |""".stripMargin)
        case _ =>
          super.traverseTree(tree)(owner)
  }.traverseTree(x.asTerm)(Symbol.spliceOwner)
  val infos = Expr(sb.toString)
  '{ println($infos) }
}
