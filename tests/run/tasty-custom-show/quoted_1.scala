import scala.quoted._

import scala.tasty.TopLevelSplice
import scala.tasty.Tasty
import scala.tasty.util.{TreeTraverser, Show}

object Macros {

  implicit rewrite def printOwners[T](x: => T): Unit =
    ~impl('(x))(TopLevelSplice.tastyContext) // FIXME infer TopLevelSplice.tastyContext within top level ~

  def impl[T](x: Expr[T])(implicit tasty: Tasty): Expr[Unit] = {
    import tasty._

    val buff = new StringBuilder

    val output = new TreeTraverser(tasty) {
      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = {
        // Use custom Show[_] here
        implicit val printer = new DummyShow(tasty)
        tree match {
          case IsDefinition(tree @ DefDef(name, _, _, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.tree.get.show)
            buff.append("\n\n")
          case IsDefinition(tree @ ValDef(name, _, _)) =>
            buff.append(name)
            buff.append("\n")
            buff.append(tree.symbol.owner.tree.get.show)
            buff.append("\n\n")
          case _ =>
        }
        traverseTreeChildren(tree)
      }
    }

    val tree = x.toTasty
    output.traverseTree(tree)
    '(print(~buff.result().toExpr))
  }

}

class DummyShow[T <: Tasty with Singleton](tasty0: T) extends Show[T](tasty0) {
  import tasty._
  def showTree(tree: Tree)(implicit ctx: Context): String = "Tree"
  def showCaseDef(caseDef: CaseDef)(implicit ctx: Context): String = "CaseDef"
  def showPattern(pattern: Pattern)(implicit ctx: Context): String = "Pattern"
  def showTypeOrBoundsTree(tpt: TypeOrBoundsTree)(implicit ctx: Context): String = "TypeOrBoundsTree"
  def showTypeOrBounds(tpe: TypeOrBounds)(implicit ctx: Context): String = "TypeOrBounds"
  def showConstant(const: Constant)(implicit ctx: Context): String = "Constant"
}
