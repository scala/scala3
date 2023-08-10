import scala.quoted.*

object Macros {

  implicit inline def printOwners[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    val buff = new StringBuilder

    val output = myTraverser(buff)

    val tree = x.asTerm
    output.traverseTree(tree)(Symbol.spliceOwner)
    '{print(${Expr(buff.result())})}
  }


  def myTraverser(using Quotes)(buff: StringBuilder): quotes.reflect.TreeTraverser = new {
    import quotes.reflect.*
    override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
      tree match {
        case tree @ DefDef(name, _, _, _) =>
          buff.append(name)
          buff.append("\n")
          buff.append(tree.symbol.owner.tree.show(using Printer.TreeStructure))
          buff.append("\n\n")
        case tree @ ValDef(name, _, _) =>
          buff.append(name)
          buff.append("\n")
          buff.append(tree.symbol.owner.tree.show(using Printer.TreeStructure))
          buff.append("\n\n")
        case _ =>
      }
      traverseTreeChildren(tree)(owner)
    }
  }

}
