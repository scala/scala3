package dotty.tools.dotc
package transform

import ast.tpd
import core.Contexts.{ctx, Context}
import core.Decorators.*
import core.Flags.*
import core.Symbols.*
import MegaPhase.MiniPhase

class CheckTermination extends MiniPhase {
  import tpd.*

  override def phaseName: String = CheckTermination.name

  override def description: String = CheckTermination.description

  override def isEnabled(using Context): Boolean = ctx.settings.YcheckTermination.value

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val method = tree.symbol
    val mandatory = method.hasAnnotation(defn.TerminationAnnot)

    if mandatory && !method.is(Deferred) then {
      val checker = new TerminationChecker(method, tree.termParamss.flatten.map(_.symbol))
      checker.traverse(tree.rhs)
    }

    tree
  }

  private class TerminationChecker(method: Symbol, params: List[Symbol])(using Context) extends TreeTraverser {

    override def traverse(tree: Tree)(using Context): Unit =
      tree match {
        case tree @ Apply(fun, args) if fun.symbol == method =>
          report.error(s"Recursive call to ${method.name}", tree.srcPos)
          traverseChildren(tree)
        case _ => traverseChildren(tree)
      }
  }
}

object CheckTermination:
  val name = "check-termination"
  val description = "check if annotated functions terminate"

