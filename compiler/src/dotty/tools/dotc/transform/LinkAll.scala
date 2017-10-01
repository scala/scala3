package dotty.tools.dotc.transform

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._

/** Loads all potentially reachable trees from tasty. ▲
 *  Only performed on whole world optimization mode. ▲ ▲
 *
 *  TODO: Next step is to only load compilation units reachable in the call graph
 */
class LinkAll extends Phase {
  import tpd._
  import LinkAll._

  def phaseName: String = "linkAll"

  def run(implicit ctx: Context): Unit = ()

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    /** Loads and processes new compilation units, possibly loading more units. */
    def allUnits(processed: Set[CompilationUnit], unprocessed: Set[CompilationUnit], loadedClasses: Set[ClassDenotation])(implicit ctx: Context): List[CompilationUnit] = {
      if (unprocessed.isEmpty) processed.toList
      else {
        val accum = new ClassesToLoadAccumulator
        val classesToLoad = unprocessed.foldLeft(Set.empty[ClassDenotation])((acc, unit) => accum.apply(acc, unit.tpdTree)) -- loadedClasses
        val loadedUnits = classesToLoad.flatMap(cls => loadCompilationUnit(cls))
        allUnits(processed ++ unprocessed, loadedUnits, loadedClasses ++ classesToLoad)
      }
    }

    if (ctx.settings.XlinkOptimise.value) allUnits(Set.empty, units.toSet, Set.empty)
    else units
  }

  /** Collects all class denotations that may need to be loaded. */
  private class ClassesToLoadAccumulator extends TreeAccumulator[Set[ClassDenotation]] {
    private var inParents = false
    override def apply(acc: Set[ClassDenotation], tree: tpd.Tree)(implicit ctx: Context): Set[ClassDenotation] = tree match {
      case New(tpt) => accum(acc, tpt.tpe.classSymbol)
      case AppliedTypeTree(tpt, _) if inParents => accum(acc, tpt.symbol)
      case tree: RefTree if inParents || tree.symbol.is(Module) =>
        foldOver(accum(acc, tree.symbol), tree)
      case tree @ Template(constr, parents, self, _) =>
        val acc1 = this(acc, constr)
        inParents = true
        val acc2 = this(acc1, parents)
        inParents = false
        this(this(acc2, self), tree.body)
      case _ => foldOver(acc, tree)
    }

    /** Accumulate class denotation for `sym` if needed */
    private def accum(acc: Set[ClassDenotation], sym: Symbol)(implicit ctx: Context): Set[ClassDenotation] = {
      val topClass = sym.topLevelClass.denot.asClass
      if (topClass.is(JavaDefined) || topClass.is(Scala2x) || topClass.symbol == defn.ObjectClass) acc
      else acc + topClass
    }
  }
}

object LinkAll {

  private[LinkAll] def loadCompilationUnit(clsd: ClassDenotation)(implicit ctx: Context): Option[CompilationUnit] = {
    assert(ctx.settings.XlinkOptimise.value)
    val tree = clsd.symbol.asClass.tree
    if (tree.isEmpty) None
    else {
      ctx.log("Loading compilation unit for: " + clsd)
      Some(CompilationUnit.mkCompilationUnit(clsd, tree, forceTrees = false))
    }
  }

}
