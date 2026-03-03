package dotty.tools.dotc
package transform

import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Phases.{Phase, postTyperPhase}
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.util.SourceFile

/** Ycheck inlined positions */
class YCheckPositions extends Phase {
  import tpd.*

  override def phaseName: String = YCheckPositions.name

  override def description: String = YCheckPositions.description

  protected def run(using Context): Unit = () // YCheck only

  override def checkPostCondition(tree: Tree)(using Context): Unit =
    tree match {
      case PackageDef(pid, _) if tree.symbol.owner == defn.RootClass =>
        val checker = new TreeTraverser {
          private var sources: List[SourceFile] = ctx.source :: Nil
          def traverse(tree: tpd.Tree)(using Context): Unit = {

            // Check current context is correct
            assert(ctx.source == sources.head)
            if (!tree.isEmpty && !tree.isInstanceOf[untpd.TypedSplice] && !tree.isInstanceOf[Inlined] && ctx.typerState.isGlobalCommittable)
              if !tree.isType // TODO also check types, currently we do not add Inlined(EmptyTree, _, _) for types. We should.
                && !tree.symbol.is(InlineProxy) // TODO check inline proxies (see tests/tun/lst)
              then
                val currentSource = sources.head
                assert(tree.source == currentSource, i"wrong source set for $tree # ${tree.uniqueId} of ${tree.getClass}, set to ${tree.source} but context had $currentSource\n ${tree.symbol.flagsString}")

            // Recursively check children while keeping track of current source
            reporting.trace(i"check pos ${tree.getClass} ${tree.source} ${sources.head} $tree") {
              tree match {
                case tree @ Inlined(_, bindings, expansion) if tree.inlinedFromOuterScope =>
                  assert(bindings.isEmpty)
                  val old = sources
                  sources = old.tail
                  traverse(expansion)(using inlineContext(tree).withSource(sources.head))
                  sources = old
                case tree @ Inlined(call, bindings, expansion) =>
                  // bindings.foreach(traverse(_)) // TODO check inline proxies (see tests/tun/lst)
                  sources = call.symbol.topLevelClass.source :: sources
                  if !isMacro(call) // FIXME macro implementations can drop Inlined nodes. We should reinsert them after macro expansion based on the positions of the trees
                    && !isBootstrappedPredefWithPatchedMethods(call) // FIXME The patched symbol has a different source as the definition of Predef. Solution: define them directly in `Predef`s TASTy and do not patch (see #19231).
                  then
                    traverse(expansion)(using inlineContext(tree).withSource(sources.head))
                  sources = sources.tail
                case _ => traverseChildren(tree)
              }
            }
          }
        }
        checker.traverse(tree)
      case _ =>
    }

  private def isBootstrappedPredefWithPatchedMethods(call: Tree)(using Context) =
    val sym = call.symbol
    (sym.is(Inline) && sym.owner == defn.ScalaPredefModuleClass && sym.owner.is(Scala2Tasty))
    || (sym == defn.ScalaPredefModuleClass && sym.is(Scala2Tasty))

  private def isMacro(call: Tree)(using Context) =
    call.symbol.is(Macro) ||
    (call.symbol.isClass && call.tpe.derivesFrom(defn.MacroAnnotationClass)) ||
    // The call of a macro after typer is encoded as a Select while other inlines are Ident
    // TODO remove this distinction once Inline nodes of expanded macros can be trusted (also in Inliner.inlineCallTrace)
    (!(ctx.phase <= postTyperPhase) && call.isInstanceOf[Select])

}

object YCheckPositions:
  val name: String = "inlinedPositions"
  val description: String = "check inlined positions"
