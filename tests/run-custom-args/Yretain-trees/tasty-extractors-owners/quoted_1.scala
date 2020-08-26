import scala.quoted._

object Macros {

  implicit inline def printOwners[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val buff = new StringBuilder

    val output = new MyTraverser(qctx.tasty)(buff)

    val tree = x.unseal
    output.traverseTree(tree)
    '{print(${Expr(buff.result())})}
  }

  class MyTraverser[R <: scala.tasty.Reflection & Singleton](val reflect: R)(buff: StringBuilder) extends scala.tasty.reflect.TreeTraverser {
    import reflect.{given _, _}
    override def traverseTree(tree: Tree): Unit = {
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
      traverseTreeChildren(tree)
    }
  }

}
