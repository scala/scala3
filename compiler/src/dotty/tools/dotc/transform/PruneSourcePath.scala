package dotty.tools.dotc
package transform

import dotty.tools.dotc.core.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Decorators.*
import dotty.tools.dotc.transform.MegaPhase.*
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.parsing.Parser

/** Prunes method and field bodies from sources loaded from sourcepath.
 *
 * This phase removes method bodies and field RHS expressions from compilation units
 * marked as `isFromSourcePath = true`, replacing them with `???`. Only elements with
 * explicit type annotations are pruned - those without types need their bodies for
 * type inference.
 *
 * Enabled by the -Yprune-sourcepath compiler flag.
 */
class PruneSourcePath extends MiniPhase{
  import untpd.*

  override def phaseName: String = PruneSourcePath.phaseName

  override def description: String = "prune method bodies from sourcepath sources"

  // We run TreeChecker only after type checking
  override def isCheckable: Boolean = false
  override def isRunnable(using Context): Boolean = true

  override def isEnabled(using Context): Boolean =
    ctx.settings.YpruneSourcepath.value

  override def runsAfter: Set[String] = Set(Parser.name)
  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    val treeMap = new untpd.UntypedTreeMap() {
      override def transform(tree: untpd.Tree)(using Context): untpd.Tree = tree match {
        case tree: DefDef => transformDefDefUntpd(tree)
        case tree: ValDef => transformValDefUntpd(tree)
        case _ => super.transform(tree)
      }
    }
    for
      unit <- units
    yield
      println(s"Pruning source path unit: ${unit.source.path} ${unit.isFromSourcePath}")
      if unit.isFromSourcePath then
        unit.untpdTree = treeMap.transform(unit.untpdTree)
      unit

  def transformDefDefUntpd(tree: DefDef)(using Context): Tree =
      val sym = tree.symbol

      // Keep these unchanged:
      // - Constructors (needed for initialization)
      // - Abstract/deferred methods (no body to prune)
      // - Macros (need full expansion)
      // - Inline methods (body needed for inlining)
      // - Methods in value classes (special semantics)
      if sym.isConstructor ||
        sym.is(Deferred) ||
        sym.is(Abstract) ||
        sym.is(Macro) ||
        sym.is(Inline) ||
        tree.rhs.isEmpty then tree
      // Only prune if there's an explicit type annotation
      // (methods without explicit types need their bodies for type inference)
      else if
        tree.tpt.isEmpty then tree
      else
        untpd.cpy.DefDef(tree)(rhs = ref(defn.Predef_undefined))

  def transformValDefUntpd(tree: ValDef)(using Context): Tree =
    val sym = tree.symbol
    // Keep these unchanged:
    // - Parameters (part of method signature)
    // - Abstract/deferred fields (no RHS to prune)
    // - Lazy vals (initialization semantics differ)
    // - Mutable vars (may have side effects)
    // - Case accessors (needed for case class functionality)
    if sym.is(Param) ||
      sym.is(Deferred) ||
      sym.is(Abstract) ||
      sym.is(Lazy) ||
      sym.is(Mutable) ||
      sym.is(CaseAccessor) ||
      tree.rhs.isEmpty then tree
      // Only prune if there's an explicit type annotation
    else if tree.tpt.isEmpty then tree
    else untpd.cpy.ValDef(tree)(rhs = ref(defn.Predef_undefined))
}

object PruneSourcePath:
  val phaseName: String = "pruneSourcePath"
