import scala.quoted._

import scala.tasty.Reflection
import scala.tasty.util.Printer

object Macros {

  implicit inline def printOwners[T](x: => T): Unit =
    ~impl('(x))

  def impl[T](x: Expr[T])(implicit reflect: Reflection): Expr[Unit] = {
    import reflect._

    val buff = new StringBuilder

    val output = new TreeTraverser {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        // Use custom Show[_] here
        val printer = new DummyShow(reflect)
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.showTree(tree))
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(printer.showTree(tree))
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.reflect
    output.traverseTree(tree)
    '(print(~buff.result().toExpr))
  }

}

class DummyShow[R <: Reflection with Singleton](reflect0: R) extends Printer[R](reflect0) {
  import reflect._
  def showTree(tree: Tree)(implicit ctx: Context): String = "Tree"
  def showCaseDef(caseDef: CaseDef)(implicit ctx: Context): String = "CaseDef"
  def showPattern(pattern: Pattern)(implicit ctx: Context): String = "Pattern"
  def showTypeOrBoundsTree(tpt: TypeOrBoundsTree)(implicit ctx: Context): String = "TypeOrBoundsTree"
  def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String = "TypeOrBounds"
  def showConstant(const: Constant)(implicit ctx: Context): String = "Constant"
  def showSymbol(symbol: Symbol)(implicit ctx: Context): String = "Symbol"
}
