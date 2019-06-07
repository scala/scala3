package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Phases
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.util.{SourceFile, SourcePosition}

/** Ycheck inlined positions */
class YCheckPositions extends Phases.Phase {
  import tpd._

  def phaseName: String = "inlinedPositions"

  override def run(implicit ctx: Context): Unit = () // YCheck only

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
        new TreeTraverser {
          private[this] var sources: List[SourceFile] = ctx.source :: Nil
          def traverse(tree: tpd.Tree)(implicit ctx: Context): Unit = {

            // Check current context is correct
            assert(ctx.source == sources.head)
            if (!tree.isEmpty && !tree.isInstanceOf[untpd.TypedSplice] && ctx.typerState.isGlobalCommittable) {
              if (!tree.isType) { // TODO also check types, currently we do not add Inlined(EmptyTree, _, _) for types. We should.
                val currentSource = sources.head
                assert(tree.source == currentSource, i"wrong source set for $tree # ${tree.uniqueId} of ${tree.getClass}, set to ${tree.source} but context had $currentSource")
              }
            }

            // Recursivlely check children while keeping track of current source
            tree match {
              case Inlined(EmptyTree, bindings, expansion) =>
                assert(bindings.isEmpty)
                val old = sources
                sources = old.tail
                traverse(expansion)(inlineContext(EmptyTree).withSource(sources.head))
                sources = old
              case Inlined(call, bindings, expansion) =>
                bindings.foreach(traverse(_))
                sources = call.symbol.topLevelClass.source :: sources
                if (!isMacro(call)) // FIXME macro implementations can drop Inlined nodes. We should reinsert them after macro expansion based on the positions of the trees
                  traverse(expansion)(inlineContext(call).withSource(sources.head))
                sources = sources.tail
              case _ => traverseChildren(tree)
            }
          }
        }.traverse(tree)
      case _ =>
    }
  }

  private def isMacro(call: Tree)(implicit ctx: Context) = {
    if (ctx.phase <= ctx.postTyperPhase) call.symbol.is(Macro)
    else call.isInstanceOf[Select] // The call of a macro after typer is encoded as a Select while other inlines are Ident
                                   // TODO remove this distinction once Inline nodes of expanded macros can be trusted (also in Inliner.inlineCallTrace)
  }

}
