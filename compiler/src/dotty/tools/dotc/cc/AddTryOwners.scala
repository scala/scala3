package dotty.tools.dotc
package cc

import core.*
import DenotTransformers.IdentityDenotTransformer
import Phases.Phase
import Contexts.*, Symbols.*, Flags.*, Types.*
import config.Feature
import ast.tpd
import StdNames.nme
import Decorators.i

object AddTryOwners:
  val name: String = "addTryOwners"
  val description: String = "add symbols for try blocks in preparation of capture checking"

class AddTryOwners extends Phase, IdentityDenotTransformer:
  thisPhase =>

  import tpd.*

  override def phaseName: String = AddTryOwners.name
  override def description: String = AddTryOwners.description

  override def isRunnable(using Context) =
    super.isRunnable && Feature.ccEnabledSomewhere

  override def isCheckable = false

  def run(using Context): Unit =
    val addTryOwners = new TreeTraverserWithPreciseImportContexts:
      def traverse(tree: Tree)(using Context): Unit = tree match
        case tree @ Try(expr, cases, finalizer) if Feature.enabled(Feature.saferExceptions) =>
          val tryOwner = newSymbol(ctx.owner, nme.TRY_BLOCK, SyntheticMethod, MethodType(Nil, defn.UnitType))
          ccState.tryBlockOwner(tree) = tryOwner
          expr.changeOwnerAfter(ctx.owner, tryOwner, thisPhase)
          inContext(ctx.withOwner(tryOwner)):
            traverse(expr)
          traverse(cases)
          traverse(finalizer)
        case _ =>
          traverseChildren(tree)
    addTryOwners.traverse(ctx.compilationUnit.tpdTree)
end AddTryOwners

