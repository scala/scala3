import scala.quoted._


object Macros {

  implicit inline def printOwners[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty._

    val buff = new StringBuilder

    val output = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        // Use custom Show[_] here
        val printer = dummyShow
        tree match {
          case tree @ DefDef(name, _, _, _, _) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.showTree(tree))
            buff.append("\n\n")
          case tree @ ValDef(name, _, _) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.showTree(tree))
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.unseal
    output.traverseTree(tree)
    '{print(${Expr(buff.result())})}
  }

  def dummyShow(using qctx: QuoteContext) : scala.tasty.reflect.Printer[qctx.tasty.type] = {
    new scala.tasty.reflect.Printer {
      val tasty = qctx.tasty
      import qctx.tasty._
      def showTree(tree: Tree)(implicit ctx: Context): String = "Tree"
      def showType(tpe: Type)(implicit ctx: Context): String = "Type"
      def showConstant(const: Constant)(implicit ctx: Context): String = "Constant"
      def showSymbol(symbol: Symbol)(implicit ctx: Context): String = "Symbol"
      def showFlags(flags: Flags)(implicit ctx: Context): String = "Flags"
    }
  }

}
