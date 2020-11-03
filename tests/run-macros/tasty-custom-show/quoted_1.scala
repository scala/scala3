import scala.quoted._


object Macros {

  implicit inline def printOwners[T](inline x: T): Unit =
    ${ impl('x) }

  def impl[T](x: Expr[T])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._

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

  def dummyShow: scala.quoted.reflect.printers.Printer = {
    new scala.quoted.reflect.printers.Printer {
      def showTree(using qctx: QuoteContext)(tree: qctx.reflect.Tree): String = "Tree"
      def showType(using qctx: QuoteContext)(tpe: qctx.reflect.TypeRepr): String = "TypeRepr"
      def showConstant(using qctx: QuoteContext)(const: qctx.reflect.Constant): String = "Constant"
      def showSymbol(using qctx: QuoteContext)(symbol: qctx.reflect.Symbol): String = "Symbol"
      def showFlags(using qctx: QuoteContext)(flags: qctx.reflect.Flags): String = "Flags"
    }
  }

}
