import scala.quoted._

object Macros {

  implicit inline def printOwners[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using Quotes) : Expr[Unit] = {
    import quotes.reflect._

    val buff = new StringBuilder

    val output = myTraverser(buff)

    val tree = Term.of(x)
    output.traverseTree(tree)(Symbol.spliceOwner)
    '{print(${Expr(buff.result())})}
  }


  def myTraverser(using Quotes)(buff: StringBuilder): quotes.reflect.TreeTraverser = new {
    import quotes.reflect._
    override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
      tree match {
        case tree @ DefDef(name, _, _, _, _) =>
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
