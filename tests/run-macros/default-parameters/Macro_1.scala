import scala.quoted.*

inline def printDefaultParameters(inline body: Unit): Unit =
  ${ printDefaultParametersExpr('body) }

def printDefaultParametersExpr(body: Expr[Unit])(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  val sb = StringBuilder()
  def addSym(sym: Symbol) =
    sb.append(
      s"""${sym.name}:
         |  ${sym.rawDefaultArgument.map(_.show)}
         |""".stripMargin)
  new TreeTraverser {
    override def traverseTree(tree: Tree)(owner: Symbol): Unit =
      tree match
        case tree: Apply =>
          for params <- tree.symbol.paramSymss
              param <- params
              if param.flags.is(Flags.HasDefault)
          do
            addSym(param)

        case tree: Definition if tree.symbol.flags.is(Flags.Param) =>
          val sym = tree.symbol
          if sym.flags.is(Flags.Param) && sym.flags.is(Flags.HasDefault) then
            addSym(sym)

        case _ =>
      super.traverseTree(tree)(owner)
  }.traverseTree(body.asTerm)(Symbol.spliceOwner)
  // println(sb.toString)
  val msg = Expr(sb.toString)
  '{ println($msg) }
