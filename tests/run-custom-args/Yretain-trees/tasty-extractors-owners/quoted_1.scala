import scala.quoted._

object Macros {

  implicit inline def printOwners[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

    val buff = new StringBuilder

    val output = myTraverser(buff)

    val tree = Term.of(x)
    output.traverseTree(tree)(Symbol.spliceOwner)
    '{print(${Expr(buff.result())})}
  }


  def myTraverser(using qctx: QuoteContext)(buff: StringBuilder): qctx.reflect.TreeTraverser = new {
    import qctx.reflect._
    override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
      tree match {
        case tree @ DefDef(name, _, _, _, _) =>
          buff.append(name)
          buff.append("\n")
          buff.append(tree.symbol.owner.tree.showExtractors)
          buff.append("\n\n")
        case tree @ ValDef(name, _, _) =>
          buff.append(name)
          buff.append("\n")
          buff.append(tree.symbol.owner.tree.showExtractors)
          buff.append("\n\n")
        case _ =>
      }
      traverseTreeChildren(tree)(owner)
    }
  }

}
