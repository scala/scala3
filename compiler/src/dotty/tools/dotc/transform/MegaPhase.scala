package dotty.tools
package dotc
package transform

import core._
import ast.Trees._
import Contexts._, Phases._, Symbols._, Decorators._
import Flags.PackageVal

/** A MegaPhase combines a number of mini-phases which are all executed in
 *  a single tree traversal.
 *
 *  This is an evolution of the previous "TreeTransformers.scala", which was written by @DarkDimius and
 *  is described in his thesis.
 */
object MegaPhase {
  import ast.tpd._

  /** The base class of tree transforms. For each kind of tree K, there are
   *  two methods which can be overridden:
   *
   *  prepareForK: return a new Context which gets passed to the node and its children
   *  transformK // transform node of type K
   *
   *  There are also prepare/transform hooks for
   *
   *   - Stats: to prepare/transform a statement sequence in a block, template, or package def,
   *   - Unit : to prepare/transform a whole compilation unit
   *   - Other: to prepape/transform a tree that does not have a specific prepare/transform
   *     method pair.
   */
  abstract class MiniPhase extends Phase {

    private[MegaPhase] var superPhase: MegaPhase = _
    private[MegaPhase] var idxInGroup: Int = _

    /** List of names of phases that should have finished their processing of all compilation units
     *  before this phase starts
     */
    def runsAfterGroupsOf: Set[String] = Set.empty

    final override def relaxedTyping: Boolean = superPhase.relaxedTyping

    /** If set, use relaxed typing for all phases in group */
    def relaxedTypingInGroup: Boolean = false

    val cpy: TypedTreeCopier = cpyBetweenPhases

    def prepareForIdent(tree: Ident)(implicit ctx: Context): Context = ctx
    def prepareForSelect(tree: Select)(implicit ctx: Context): Context = ctx
    def prepareForThis(tree: This)(implicit ctx: Context): Context = ctx
    def prepareForSuper(tree: Super)(implicit ctx: Context): Context = ctx
    def prepareForApply(tree: Apply)(implicit ctx: Context): Context = ctx
    def prepareForTypeApply(tree: TypeApply)(implicit ctx: Context): Context = ctx
    def prepareForLiteral(tree: Literal)(implicit ctx: Context): Context = ctx
    def prepareForNew(tree: New)(implicit ctx: Context): Context = ctx
    def prepareForTyped(tree: Typed)(implicit ctx: Context): Context = ctx
    def prepareForAssign(tree: Assign)(implicit ctx: Context): Context = ctx
    def prepareForBlock(tree: Block)(implicit ctx: Context): Context = ctx
    def prepareForIf(tree: If)(implicit ctx: Context): Context = ctx
    def prepareForClosure(tree: Closure)(implicit ctx: Context): Context = ctx
    def prepareForMatch(tree: Match)(implicit ctx: Context): Context = ctx
    def prepareForCaseDef(tree: CaseDef)(implicit ctx: Context): Context = ctx
    def prepareForLabeled(tree: Labeled)(implicit ctx: Context): Context = ctx
    def prepareForReturn(tree: Return)(implicit ctx: Context): Context = ctx
    def prepareForWhileDo(tree: WhileDo)(implicit ctx: Context): Context = ctx
    def prepareForTry(tree: Try)(implicit ctx: Context): Context = ctx
    def prepareForSeqLiteral(tree: SeqLiteral)(implicit ctx: Context): Context = ctx
    def prepareForInlined(tree: Inlined)(implicit ctx: Context): Context = ctx
    def prepareForTypeTree(tree: TypeTree)(implicit ctx: Context): Context = ctx
    def prepareForBind(tree: Bind)(implicit ctx: Context): Context = ctx
    def prepareForAlternative(tree: Alternative)(implicit ctx: Context): Context = ctx
    def prepareForUnApply(tree: UnApply)(implicit ctx: Context): Context = ctx
    def prepareForValDef(tree: ValDef)(implicit ctx: Context): Context = ctx
    def prepareForDefDef(tree: DefDef)(implicit ctx: Context): Context = ctx
    def prepareForTypeDef(tree: TypeDef)(implicit ctx: Context): Context = ctx
    def prepareForTemplate(tree: Template)(implicit ctx: Context): Context = ctx
    def prepareForPackageDef(tree: PackageDef)(implicit ctx: Context): Context = ctx
    def prepareForStats(trees: List[Tree])(implicit ctx: Context): Context = ctx
    def prepareForUnit(tree: Tree)(implicit ctx: Context): Context = ctx
    def prepareForOther(tree: Tree)(implicit ctx: Context): Context = ctx

    def transformIdent(tree: Ident)(implicit ctx: Context): Tree = tree
    def transformSelect(tree: Select)(implicit ctx: Context): Tree = tree
    def transformThis(tree: This)(implicit ctx: Context): Tree = tree
    def transformSuper(tree: Super)(implicit ctx: Context): Tree = tree
    def transformApply(tree: Apply)(implicit ctx: Context): Tree = tree
    def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = tree
    def transformLiteral(tree: Literal)(implicit ctx: Context): Tree = tree
    def transformNew(tree: New)(implicit ctx: Context): Tree = tree
    def transformTyped(tree: Typed)(implicit ctx: Context): Tree = tree
    def transformAssign(tree: Assign)(implicit ctx: Context): Tree = tree
    def transformBlock(tree: Block)(implicit ctx: Context): Tree = tree
    def transformIf(tree: If)(implicit ctx: Context): Tree = tree
    def transformClosure(tree: Closure)(implicit ctx: Context): Tree = tree
    def transformMatch(tree: Match)(implicit ctx: Context): Tree = tree
    def transformCaseDef(tree: CaseDef)(implicit ctx: Context): Tree = tree
    def transformLabeled(tree: Labeled)(implicit ctx: Context): Tree = tree
    def transformReturn(tree: Return)(implicit ctx: Context): Tree = tree
    def transformWhileDo(tree: WhileDo)(implicit ctx: Context): Tree = tree
    def transformTry(tree: Try)(implicit ctx: Context): Tree = tree
    def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context): Tree = tree
    def transformInlined(tree: Inlined)(implicit ctx: Context): Tree = tree
    def transformTypeTree(tree: TypeTree)(implicit ctx: Context): Tree = tree
    def transformBind(tree: Bind)(implicit ctx: Context): Tree = tree
    def transformAlternative(tree: Alternative)(implicit ctx: Context): Tree = tree
    def transformUnApply(tree: UnApply)(implicit ctx: Context): Tree = tree
    def transformValDef(tree: ValDef)(implicit ctx: Context): Tree = tree
    def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree = tree
    def transformTypeDef(tree: TypeDef)(implicit ctx: Context): Tree = tree
    def transformTemplate(tree: Template)(implicit ctx: Context): Tree = tree
    def transformPackageDef(tree: PackageDef)(implicit ctx: Context): Tree = tree
    def transformStats(trees: List[Tree])(implicit ctx: Context): List[Tree] = trees
    def transformUnit(tree: Tree)(implicit ctx: Context): Tree = tree
    def transformOther(tree: Tree)(implicit ctx: Context): Tree = tree

    /** Transform tree using all transforms of current group (including this one) */
    def transformAllDeep(tree: Tree)(implicit ctx: Context): Tree =
      superPhase.transformTree(tree, 0)

    /** Transform tree using all transforms following the current one in this group */
    def transformFollowingDeep(tree: Tree)(implicit ctx: Context): Tree =
      superPhase.transformTree(tree, idxInGroup + 1)

    /** Transform single node using all transforms following the current one in this group */
    def transformFollowing(tree: Tree)(implicit ctx: Context): Tree =
      superPhase.transformNode(tree, idxInGroup + 1)

    protected def singletonGroup: MegaPhase = new MegaPhase(Array(this))

    override def run(implicit ctx: Context): Unit =
      singletonGroup.run
  }
}
import MegaPhase._

class MegaPhase(val miniPhases: Array[MiniPhase]) extends Phase {
  import ast.tpd._

  override val phaseName: String =
    if (miniPhases.length == 1) miniPhases(0).phaseName
    else miniPhases.map(_.phaseName).mkString("MegaPhase{", ", ", "}")

  private var relaxedTypingCache: Boolean = _
  private var relaxedTypingKnown = false

  override final def relaxedTyping: Boolean = {
    if (!relaxedTypingKnown) {
      relaxedTypingCache = miniPhases.exists(_.relaxedTypingInGroup)
      relaxedTypingKnown = true
    }
    relaxedTypingCache
  }

  private val cpy: TypedTreeCopier = cpyBetweenPhases

  /** Transform node using all phases in this group that have idxInGroup >= start */
  def transformNode(tree: Tree, start: Int)(implicit ctx: Context): Tree = {
    def goNamed(tree: Tree, start: Int) =
      try
        tree match {
          case tree: Ident => goIdent(tree, start)
          case tree: Select => goSelect(tree, start)
          case tree: ValDef => goValDef(tree, start)
          case tree: DefDef => goDefDef(tree, start)
          case tree: TypeDef => goTypeDef(tree, start)
          case tree: Labeled => goLabeled(tree, start)
          case tree: Bind => goBind(tree, start)
          case _ => goOther(tree, start)
        }
      catch {
        case ex: TypeError =>
          ctx.error(ex.toMessage, tree.sourcePos)
          tree
      }
    def goUnnamed(tree: Tree, start: Int) =
      try
        tree match {
          case tree: Apply => goApply(tree, start)
          case tree: TypeTree => goTypeTree(tree, start)
          case tree: Thicket =>
            cpy.Thicket(tree)(tree.trees.mapConserve(transformNode(_, start)))
          case tree: This => goThis(tree, start)
          case tree: Literal => goLiteral(tree, start)
          case tree: Block => goBlock(tree, start)
          case tree: TypeApply => goTypeApply(tree, start)
          case tree: If => goIf(tree, start)
          case tree: New => goNew(tree, start)
          case tree: Typed => goTyped(tree, start)
          case tree: CaseDef => goCaseDef(tree, start)
          case tree: Closure => goClosure(tree, start)
          case tree: Assign => goAssign(tree, start)
          case tree: SeqLiteral => goSeqLiteral(tree, start)
          case tree: Super => goSuper(tree, start)
          case tree: Template => goTemplate(tree, start)
          case tree: Match => goMatch(tree, start)
          case tree: UnApply => goUnApply(tree, start)
          case tree: PackageDef => goPackageDef(tree, start)
          case tree: Try => goTry(tree, start)
          case tree: Inlined => goInlined(tree, start)
          case tree: Return => goReturn(tree, start)
          case tree: WhileDo => goWhileDo(tree, start)
          case tree: Alternative => goAlternative(tree, start)
          case tree => goOther(tree, start)
        }
      catch {
        case ex: TypeError =>
          ctx.error(ex.toMessage, tree.sourcePos, sticky = true)
          tree
      }
    if (tree.isInstanceOf[NameTree]) goNamed(tree, start) else goUnnamed(tree, start)
  }

  /** Transform full tree using all phases in this group that have idxInGroup >= start */
  def transformTree(tree: Tree, start: Int)(implicit ctx: Context): Tree = {
    def localContext(implicit ctx: Context) = {
      val sym = tree.symbol
      val owner = if (sym is PackageVal) sym.moduleClass else sym
      ctx.fresh.setOwner(owner)
    }

    def transformNamed(tree: Tree, start: Int, outerCtx: Context): Tree = tree match {
      case tree: Ident =>
        implicit val ctx = prepIdent(tree, start)(outerCtx)
        goIdent(tree, start)
      case tree: Select =>
        implicit val ctx = prepSelect(tree, start)(outerCtx)
        val qual = transformTree(tree.qualifier, start)
        goSelect(cpy.Select(tree)(qual, tree.name), start)
      case tree: ValDef =>
        implicit val ctx = prepValDef(tree, start)(outerCtx)
        def mapValDef(implicit ctx: Context) = {
          val tpt = transformTree(tree.tpt, start)
          val rhs = transformTree(tree.rhs, start)
          cpy.ValDef(tree)(tree.name, tpt, rhs)
        }
        if (tree.isEmpty) tree
        else goValDef(mapValDef(if (tree.symbol.exists) localContext else ctx), start)
      case tree: DefDef =>
        implicit val ctx = prepDefDef(tree, start)(outerCtx)
        def mapDefDef(implicit ctx: Context) = {
          val tparams = transformSpecificTrees(tree.tparams, start)
          val vparamss = tree.vparamss.mapConserve(transformSpecificTrees(_, start))
          val tpt = transformTree(tree.tpt, start)
          val rhs = transformTree(tree.rhs, start)
          cpy.DefDef(tree)(tree.name, tparams, vparamss, tpt, rhs)
        }
        goDefDef(mapDefDef(localContext), start)
      case tree: TypeDef =>
        implicit val ctx = prepTypeDef(tree, start)(outerCtx)
        val rhs = transformTree(tree.rhs, start)(localContext)
        goTypeDef(cpy.TypeDef(tree)(tree.name, rhs), start)
      case tree: Labeled =>
        implicit val ctx = prepLabeled(tree, start)(outerCtx)
        val bind = transformTree(tree.bind, start).asInstanceOf[Bind]
        val expr = transformTree(tree.expr, start)
        goLabeled(cpy.Labeled(tree)(bind, expr), start)
      case tree: Bind =>
        implicit val ctx = prepBind(tree, start)(outerCtx)
        val body = transformTree(tree.body, start)
        goBind(cpy.Bind(tree)(tree.name, body), start)
      case _ =>
        implicit val ctx = prepOther(tree, start)(outerCtx)
        goOther(tree, start)
    }

    def transformUnnamed(tree: Tree, start: Int, outerCtx: Context): Tree = tree match {
      case tree: Apply =>
        implicit val ctx = prepApply(tree, start)(outerCtx)
        val fun = transformTree(tree.fun, start)
        val args = transformTrees(tree.args, start)
        goApply(cpy.Apply(tree)(fun, args), start)
      case tree: TypeTree =>
        implicit val ctx = prepTypeTree(tree, start)(outerCtx)
        goTypeTree(tree, start)
      case tree: Thicket =>
        cpy.Thicket(tree)(transformTrees(tree.trees, start))
      case tree: This =>
        implicit val ctx = prepThis(tree, start)(outerCtx)
        goThis(tree, start)
      case tree: Literal =>
        implicit val ctx = prepLiteral(tree, start)(outerCtx)
        goLiteral(tree, start)
      case tree: Block =>
        implicit val ctx = prepBlock(tree, start)(outerCtx)
        val stats = transformStats(tree.stats, ctx.owner, start)
        val expr = transformTree(tree.expr, start)
        goBlock(cpy.Block(tree)(stats, expr), start)
      case tree: TypeApply =>
        implicit val ctx = prepTypeApply(tree, start)(outerCtx)
        val fun = transformTree(tree.fun, start)
        val args = transformTrees(tree.args, start)
        goTypeApply(cpy.TypeApply(tree)(fun, args), start)
      case tree: If =>
        implicit val ctx = prepIf(tree, start)(outerCtx)
        val cond = transformTree(tree.cond, start)
        val thenp = transformTree(tree.thenp, start)
        val elsep = transformTree(tree.elsep, start)
        goIf(cpy.If(tree)(cond, thenp, elsep), start)
      case tree: New =>
        implicit val ctx = prepNew(tree, start)(outerCtx)
        val tpt = transformTree(tree.tpt, start)
        goNew(cpy.New(tree)(tpt), start)
      case tree: Typed =>
        implicit val ctx = prepTyped(tree, start)(outerCtx)
        val expr = transformTree(tree.expr, start)
        val tpt = transformTree(tree.tpt, start)
        goTyped(cpy.Typed(tree)(expr, tpt), start)
      case tree: CaseDef =>
        implicit val ctx = prepCaseDef(tree, start)(outerCtx)
        val pat = transformTree(tree.pat, start)(ctx.addMode(Mode.Pattern))
        val guard = transformTree(tree.guard, start)
        val body = transformTree(tree.body, start)
        goCaseDef(cpy.CaseDef(tree)(pat, guard, body), start)
      case tree: Closure =>
        implicit val ctx = prepClosure(tree, start)(outerCtx)
        val env = transformTrees(tree.env, start)
        val meth = transformTree(tree.meth, start)
        val tpt = transformTree(tree.tpt, start)
        goClosure(cpy.Closure(tree)(env, meth, tpt), start)
      case tree: Assign =>
        implicit val ctx = prepAssign(tree, start)(outerCtx)
        val lhs = transformTree(tree.lhs, start)
        val rhs = transformTree(tree.rhs, start)
        goAssign(cpy.Assign(tree)(lhs, rhs), start)
      case tree: SeqLiteral =>
        implicit val ctx = prepSeqLiteral(tree, start)(outerCtx)
        val elems = transformTrees(tree.elems, start)
        val elemtpt = transformTree(tree.elemtpt, start)
        goSeqLiteral(cpy.SeqLiteral(tree)(elems, elemtpt), start)
      case tree: Super =>
        implicit val ctx = prepSuper(tree, start)(outerCtx)
        goSuper(tree, start)
      case tree: Template =>
        implicit val ctx = prepTemplate(tree, start)(outerCtx)
        val constr = transformSpecificTree(tree.constr, start)
        val parents = transformTrees(tree.parents, start)(ctx.superCallContext)
        val self = transformSpecificTree(tree.self, start)
        val body = transformStats(tree.body, tree.symbol, start)
        goTemplate(cpy.Template(tree)(constr, parents, Nil, self, body), start)
      case tree: Match =>
        implicit val ctx = prepMatch(tree, start)(outerCtx)
        val selector = transformTree(tree.selector, start)
        val cases = transformSpecificTrees(tree.cases, start)
        goMatch(cpy.Match(tree)(selector, cases), start)
      case tree: UnApply =>
        implicit val ctx = prepUnApply(tree, start)(outerCtx)
        val fun = transformTree(tree.fun, start)
        val implicits = transformTrees(tree.implicits, start)
        val patterns = transformTrees(tree.patterns, start)
        goUnApply(cpy.UnApply(tree)(fun, implicits, patterns), start)
      case tree: PackageDef =>
        implicit val ctx = prepPackageDef(tree, start)(outerCtx)
        def mapPackage(implicit ctx: Context) = {
          val pid = transformSpecificTree(tree.pid, start)
          val stats = transformStats(tree.stats, tree.symbol, start)
          cpy.PackageDef(tree)(pid, stats)
        }
        goPackageDef(mapPackage(localContext), start)
      case tree: Try =>
        implicit val ctx = prepTry(tree, start)(outerCtx)
        val expr = transformTree(tree.expr, start)
        val cases = transformSpecificTrees(tree.cases, start)
        val finalizer = transformTree(tree.finalizer, start)
        goTry(cpy.Try(tree)(expr, cases, finalizer), start)
      case tree: Inlined =>
        implicit val ctx = prepInlined(tree, start)(outerCtx)
        val bindings = transformSpecificTrees(tree.bindings, start)
        val expansion = transformTree(tree.expansion, start)(inlineContext(tree.call))
        goInlined(cpy.Inlined(tree)(tree.call, bindings, expansion), start)
      case tree: Return =>
        implicit val ctx = prepReturn(tree, start)(outerCtx)
        val expr = transformTree(tree.expr, start)
        goReturn(cpy.Return(tree)(expr, tree.from), start)
          // don't transform `tree.from`, as this is not a normal ident, but
          // a pointer to the enclosing method.
      case tree: WhileDo =>
        implicit val ctx = prepWhileDo(tree, start)(outerCtx)
        val cond = transformTree(tree.cond, start)
        val body = transformTree(tree.body, start)
        goWhileDo(cpy.WhileDo(tree)(cond, body), start)
      case tree: Alternative =>
        implicit val ctx = prepAlternative(tree, start)(outerCtx)
        val trees = transformTrees(tree.trees, start)
        goAlternative(cpy.Alternative(tree)(trees), start)
      case tree =>
        implicit val ctx = prepOther(tree, start)(outerCtx)
        goOther(tree, start)
    }

    if (tree.source != ctx.source && tree.source.exists)
      transformTree(tree, start)(ctx.withSource(tree.source))
    else if (tree.isInstanceOf[NameTree])
      transformNamed(tree, start, ctx)
    else
      transformUnnamed(tree, start, ctx)
  }

  def transformSpecificTree[T <: Tree](tree: T, start: Int)(implicit ctx: Context): T =
    transformTree(tree, start).asInstanceOf[T]

  def transformStats(trees: List[Tree], exprOwner: Symbol, start: Int)(implicit ctx: Context): List[Tree] = {
    def transformStat(stat: Tree)(implicit ctx: Context): Tree = stat match {
      case _: Import | _: DefTree => transformTree(stat, start)
      case Thicket(stats) => cpy.Thicket(stat)(stats.mapConserve(transformStat))
      case _ => transformTree(stat, start)(ctx.exprContext(stat, exprOwner))
    }
    val nestedCtx = prepStats(trees, start)(ctx)
    val trees1 = flatten(trees.mapConserve(transformStat(_)(nestedCtx)))
    goStats(trees1, start)(nestedCtx)
  }

  def transformUnit(tree: Tree)(implicit ctx: Context): Tree = {
    val nestedCtx = prepUnit(tree, 0)(ctx)
    val tree1 = transformTree(tree, 0)(nestedCtx)
    goUnit(tree1, 0)(nestedCtx)
  }

  def transformTrees(trees: List[Tree], start: Int)(implicit ctx: Context): List[Tree] =
    flatten(trees.mapConserve(transformTree(_, start)))

  def transformSpecificTrees[T <: Tree](trees: List[T], start: Int)(implicit ctx: Context): List[T] =
    transformTrees(trees, start).asInstanceOf[List[T]]

  override def run(implicit ctx: Context): Unit =
    ctx.compilationUnit.tpdTree =
      transformUnit(ctx.compilationUnit.tpdTree)(ctx.withPhase(miniPhases.last.next))

  // Initialization code

  for ((phase, idx) <- miniPhases.zipWithIndex) {
    phase.superPhase = this
    phase.idxInGroup = idx
  }

  /** Class#getDeclaredMethods is slow, so we cache its output */
  private val clsMethodsCache = new java.util.IdentityHashMap[Class[_], Array[java.lang.reflect.Method]]

  /** Does `phase` contain a redefinition of method `name`?
   *  (which is a method of MiniPhase)
   */
  private def defines(phase: MiniPhase, name: String) = {
    def hasRedefinedMethod(cls: Class[_]): Boolean =
      if (cls.eq(classOf[MiniPhase])) false
      else {
        var clsMethods = clsMethodsCache.get(cls)
        if (clsMethods eq null) {
          clsMethods = cls.getDeclaredMethods
          clsMethodsCache.put(cls, clsMethods)
        }
        clsMethods.exists(_.getName == name) ||
        hasRedefinedMethod(cls.getSuperclass)
      }
    hasRedefinedMethod(phase.getClass)
  }

  private def newNxArray = new Array[MiniPhase](miniPhases.length + 1)
  private val emptyNxArray = newNxArray

  private def init(methName: String): Array[MiniPhase] = {
    var nx: Array[MiniPhase] = emptyNxArray
    for (idx <- miniPhases.length - 1 to 0 by -1) {
      val subPhase = miniPhases(idx)
      if (defines(subPhase, methName)) {
        if (nx eq emptyNxArray) nx = newNxArray
        nx(idx) = subPhase
      }
      else if (nx ne emptyNxArray) nx(idx) = nx(idx + 1)
    }
    nx
  }

  private val nxIdentPrepPhase = init("prepareForIdent")
  private val nxIdentTransPhase = init("transformIdent")
  private val nxSelectPrepPhase = init("prepareForSelect")
  private val nxSelectTransPhase = init("transformSelect")
  private val nxThisPrepPhase = init("prepareForThis")
  private val nxThisTransPhase = init("transformThis")
  private val nxSuperPrepPhase = init("prepareForSuper")
  private val nxSuperTransPhase = init("transformSuper")
  private val nxApplyPrepPhase = init("prepareForApply")
  private val nxApplyTransPhase = init("transformApply")
  private val nxTypeApplyPrepPhase = init("prepareForTypeApply")
  private val nxTypeApplyTransPhase = init("transformTypeApply")
  private val nxLiteralPrepPhase = init("prepareForLiteral")
  private val nxLiteralTransPhase = init("transformLiteral")
  private val nxNewPrepPhase = init("prepareForNew")
  private val nxNewTransPhase = init("transformNew")
  private val nxTypedPrepPhase = init("prepareForTyped")
  private val nxTypedTransPhase = init("transformTyped")
  private val nxAssignPrepPhase = init("prepareForAssign")
  private val nxAssignTransPhase = init("transformAssign")
  private val nxBlockPrepPhase = init("prepareForBlock")
  private val nxBlockTransPhase = init("transformBlock")
  private val nxIfPrepPhase = init("prepareForIf")
  private val nxIfTransPhase = init("transformIf")
  private val nxClosurePrepPhase = init("prepareForClosure")
  private val nxClosureTransPhase = init("transformClosure")
  private val nxMatchPrepPhase = init("prepareForMatch")
  private val nxMatchTransPhase = init("transformMatch")
  private val nxCaseDefPrepPhase = init("prepareForCaseDef")
  private val nxCaseDefTransPhase = init("transformCaseDef")
  private val nxLabeledPrepPhase = init("prepareForLabeled")
  private val nxLabeledTransPhase = init("transformLabeled")
  private val nxReturnPrepPhase = init("prepareForReturn")
  private val nxReturnTransPhase = init("transformReturn")
  private val nxWhileDoPrepPhase = init("prepareForWhileDo")
  private val nxWhileDoTransPhase = init("transformWhileDo")
  private val nxTryPrepPhase = init("prepareForTry")
  private val nxTryTransPhase = init("transformTry")
  private val nxSeqLiteralPrepPhase = init("prepareForSeqLiteral")
  private val nxSeqLiteralTransPhase = init("transformSeqLiteral")
  private val nxInlinedPrepPhase = init("prepareForInlined")
  private val nxInlinedTransPhase = init("transformInlined")
  private val nxTypeTreePrepPhase = init("prepareForTypeTree")
  private val nxTypeTreeTransPhase = init("transformTypeTree")
  private val nxBindPrepPhase = init("prepareForBind")
  private val nxBindTransPhase = init("transformBind")
  private val nxAlternativePrepPhase = init("prepareForAlternative")
  private val nxAlternativeTransPhase = init("transformAlternative")
  private val nxUnApplyPrepPhase = init("prepareForUnApply")
  private val nxUnApplyTransPhase = init("transformUnApply")
  private val nxValDefPrepPhase = init("prepareForValDef")
  private val nxValDefTransPhase = init("transformValDef")
  private val nxDefDefPrepPhase = init("prepareForDefDef")
  private val nxDefDefTransPhase = init("transformDefDef")
  private val nxTypeDefPrepPhase = init("prepareForTypeDef")
  private val nxTypeDefTransPhase = init("transformTypeDef")
  private val nxTemplatePrepPhase = init("prepareForTemplate")
  private val nxTemplateTransPhase = init("transformTemplate")
  private val nxPackageDefPrepPhase = init("prepareForPackageDef")
  private val nxPackageDefTransPhase = init("transformPackageDef")
  private val nxStatsPrepPhase = init("prepareForStats")
  private val nxStatsTransPhase = init("transformStats")
  private val nxUnitPrepPhase = init("prepareForUnit")
  private val nxUnitTransPhase = init("transformUnit")
  private val nxOtherPrepPhase = init("prepareForOther")
  private val nxOtherTransPhase = init("transformOther")

  // Boilerplate snippets

  def prepIdent(tree: Ident, start: Int)(implicit ctx: Context): Context = {
    val phase = nxIdentPrepPhase(start)
    if (phase == null) ctx
    else prepIdent(tree, phase.idxInGroup + 1)(phase.prepareForIdent(tree))
  }

  def goIdent(tree: Ident, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxIdentTransPhase(start)
    if (phase == null) tree
    else phase.transformIdent(tree)(ctx) match {
      case tree1: Ident => goIdent(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSelect(tree: Select, start: Int)(implicit ctx: Context): Context = {
    val phase = nxSelectPrepPhase(start)
    if (phase == null) ctx
    else prepSelect(tree, phase.idxInGroup + 1)(phase.prepareForSelect(tree))
  }

  def goSelect(tree: Select, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxSelectTransPhase(start)
    if (phase == null) tree
    else phase.transformSelect(tree)(ctx) match {
      case tree1: Select => goSelect(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepThis(tree: This, start: Int)(implicit ctx: Context): Context = {
    val phase = nxThisPrepPhase(start)
    if (phase == null) ctx
    else prepThis(tree, phase.idxInGroup + 1)(phase.prepareForThis(tree))
  }

  def goThis(tree: This, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxThisTransPhase(start)
    if (phase == null) tree
    else phase.transformThis(tree)(ctx) match {
      case tree1: This => goThis(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSuper(tree: Super, start: Int)(implicit ctx: Context): Context = {
    val phase = nxSuperPrepPhase(start)
    if (phase == null) ctx
    else prepSuper(tree, phase.idxInGroup + 1)(phase.prepareForSuper(tree))
  }

  def goSuper(tree: Super, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxSuperTransPhase(start)
    if (phase == null) tree
    else phase.transformSuper(tree)(ctx) match {
      case tree1: Super => goSuper(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepApply(tree: Apply, start: Int)(implicit ctx: Context): Context = {
    val phase = nxApplyPrepPhase(start)
    if (phase == null) ctx
    else prepApply(tree, phase.idxInGroup + 1)(phase.prepareForApply(tree))
  }

  def goApply(tree: Apply, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxApplyTransPhase(start)
    if (phase == null) tree
    else phase.transformApply(tree)(ctx) match {
      case tree1: Apply => goApply(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTypeApply(tree: TypeApply, start: Int)(implicit ctx: Context): Context = {
    val phase = nxTypeApplyPrepPhase(start)
    if (phase == null) ctx
    else prepTypeApply(tree, phase.idxInGroup + 1)(phase.prepareForTypeApply(tree))
  }

  def goTypeApply(tree: TypeApply, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxTypeApplyTransPhase(start)
    if (phase == null) tree
    else phase.transformTypeApply(tree)(ctx) match {
      case tree1: TypeApply => goTypeApply(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepLiteral(tree: Literal, start: Int)(implicit ctx: Context): Context = {
    val phase = nxLiteralPrepPhase(start)
    if (phase == null) ctx
    else prepLiteral(tree, phase.idxInGroup + 1)(phase.prepareForLiteral(tree))
  }

  def goLiteral(tree: Literal, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxLiteralTransPhase(start)
    if (phase == null) tree
    else phase.transformLiteral(tree)(ctx) match {
      case tree1: Literal => goLiteral(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepNew(tree: New, start: Int)(implicit ctx: Context): Context = {
    val phase = nxNewPrepPhase(start)
    if (phase == null) ctx
    else prepNew(tree, phase.idxInGroup + 1)(phase.prepareForNew(tree))
  }

  def goNew(tree: New, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxNewTransPhase(start)
    if (phase == null) tree
    else phase.transformNew(tree)(ctx) match {
      case tree1: New => goNew(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTyped(tree: Typed, start: Int)(implicit ctx: Context): Context = {
    val phase = nxTypedPrepPhase(start)
    if (phase == null) ctx
    else prepTyped(tree, phase.idxInGroup + 1)(phase.prepareForTyped(tree))
  }

  def goTyped(tree: Typed, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxTypedTransPhase(start)
    if (phase == null) tree
    else phase.transformTyped(tree)(ctx) match {
      case tree1: Typed => goTyped(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepAssign(tree: Assign, start: Int)(implicit ctx: Context): Context = {
    val phase = nxAssignPrepPhase(start)
    if (phase == null) ctx
    else prepAssign(tree, phase.idxInGroup + 1)(phase.prepareForAssign(tree))
  }

  def goAssign(tree: Assign, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxAssignTransPhase(start)
    if (phase == null) tree
    else phase.transformAssign(tree)(ctx) match {
      case tree1: Assign => goAssign(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepBlock(tree: Block, start: Int)(implicit ctx: Context): Context = {
    val phase = nxBlockPrepPhase(start)
    if (phase == null) ctx
    else prepBlock(tree, phase.idxInGroup + 1)(phase.prepareForBlock(tree))
  }

  def goBlock(tree: Block, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxBlockTransPhase(start)
    if (phase == null) tree
    else phase.transformBlock(tree)(ctx) match {
      case tree1: Block => goBlock(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepIf(tree: If, start: Int)(implicit ctx: Context): Context = {
    val phase = nxIfPrepPhase(start)
    if (phase == null) ctx
    else prepIf(tree, phase.idxInGroup + 1)(phase.prepareForIf(tree))
  }

  def goIf(tree: If, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxIfTransPhase(start)
    if (phase == null) tree
    else phase.transformIf(tree)(ctx) match {
      case tree1: If => goIf(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepClosure(tree: Closure, start: Int)(implicit ctx: Context): Context = {
    val phase = nxClosurePrepPhase(start)
    if (phase == null) ctx
    else prepClosure(tree, phase.idxInGroup + 1)(phase.prepareForClosure(tree))
  }

  def goClosure(tree: Closure, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxClosureTransPhase(start)
    if (phase == null) tree
    else phase.transformClosure(tree)(ctx) match {
      case tree1: Closure => goClosure(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepMatch(tree: Match, start: Int)(implicit ctx: Context): Context = {
    val phase = nxMatchPrepPhase(start)
    if (phase == null) ctx
    else prepMatch(tree, phase.idxInGroup + 1)(phase.prepareForMatch(tree))
  }

  def goMatch(tree: Match, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxMatchTransPhase(start)
    if (phase == null) tree
    else phase.transformMatch(tree)(ctx) match {
      case tree1: Match => goMatch(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepCaseDef(tree: CaseDef, start: Int)(implicit ctx: Context): Context = {
    val phase = nxCaseDefPrepPhase(start)
    if (phase == null) ctx
    else prepCaseDef(tree, phase.idxInGroup + 1)(phase.prepareForCaseDef(tree))
  }

  def goCaseDef(tree: CaseDef, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxCaseDefTransPhase(start)
    if (phase == null) tree
    else phase.transformCaseDef(tree)(ctx) match {
      case tree1: CaseDef => goCaseDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepLabeled(tree: Labeled, start: Int)(implicit ctx: Context): Context = {
    val phase = nxLabeledPrepPhase(start)
    if (phase == null) ctx
    else prepLabeled(tree, phase.idxInGroup + 1)(phase.prepareForLabeled(tree))
  }

  def goLabeled(tree: Labeled, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxLabeledTransPhase(start)
    if (phase == null) tree
    else phase.transformLabeled(tree)(ctx) match {
      case tree1: Labeled => goLabeled(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepReturn(tree: Return, start: Int)(implicit ctx: Context): Context = {
    val phase = nxReturnPrepPhase(start)
    if (phase == null) ctx
    else prepReturn(tree, phase.idxInGroup + 1)(phase.prepareForReturn(tree))
  }

  def goReturn(tree: Return, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxReturnTransPhase(start)
    if (phase == null) tree
    else phase.transformReturn(tree)(ctx) match {
      case tree1: Return => goReturn(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepWhileDo(tree: WhileDo, start: Int)(implicit ctx: Context): Context = {
    val phase = nxWhileDoPrepPhase(start)
    if (phase == null) ctx
    else prepWhileDo(tree, phase.idxInGroup + 1)(phase.prepareForWhileDo(tree))
  }

  def goWhileDo(tree: WhileDo, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxWhileDoTransPhase(start)
    if (phase == null) tree
    else phase.transformWhileDo(tree)(ctx) match {
      case tree1: WhileDo => goWhileDo(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTry(tree: Try, start: Int)(implicit ctx: Context): Context = {
    val phase = nxTryPrepPhase(start)
    if (phase == null) ctx
    else prepTry(tree, phase.idxInGroup + 1)(phase.prepareForTry(tree))
  }

  def goTry(tree: Try, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxTryTransPhase(start)
    if (phase == null) tree
    else phase.transformTry(tree)(ctx) match {
      case tree1: Try => goTry(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepSeqLiteral(tree: SeqLiteral, start: Int)(implicit ctx: Context): Context = {
    val phase = nxSeqLiteralPrepPhase(start)
    if (phase == null) ctx
    else prepSeqLiteral(tree, phase.idxInGroup + 1)(phase.prepareForSeqLiteral(tree))
  }

  def goSeqLiteral(tree: SeqLiteral, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxSeqLiteralTransPhase(start)
    if (phase == null) tree
    else phase.transformSeqLiteral(tree)(ctx) match {
      case tree1: SeqLiteral => goSeqLiteral(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepInlined(tree: Inlined, start: Int)(implicit ctx: Context): Context = {
    val phase = nxInlinedPrepPhase(start)
    if (phase == null) ctx
    else prepInlined(tree, phase.idxInGroup + 1)(phase.prepareForInlined(tree))
  }

  def goInlined(tree: Inlined, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxInlinedTransPhase(start)
    if (phase == null) tree
    else phase.transformInlined(tree)(ctx) match {
      case tree1: Inlined => goInlined(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTypeTree(tree: TypeTree, start: Int)(implicit ctx: Context): Context = {
    val phase = nxTypeTreePrepPhase(start)
    if (phase == null) ctx
    else prepTypeTree(tree, phase.idxInGroup + 1)(phase.prepareForTypeTree(tree))
  }

  def goTypeTree(tree: TypeTree, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxTypeTreeTransPhase(start)
    if (phase == null) tree
    else phase.transformTypeTree(tree)(ctx) match {
      case tree1: TypeTree => goTypeTree(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepBind(tree: Bind, start: Int)(implicit ctx: Context): Context = {
    val phase = nxBindPrepPhase(start)
    if (phase == null) ctx
    else prepBind(tree, phase.idxInGroup + 1)(phase.prepareForBind(tree))
  }

  def goBind(tree: Bind, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxBindTransPhase(start)
    if (phase == null) tree
    else phase.transformBind(tree)(ctx) match {
      case tree1: Bind => goBind(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepAlternative(tree: Alternative, start: Int)(implicit ctx: Context): Context = {
    val phase = nxAlternativePrepPhase(start)
    if (phase == null) ctx
    else prepAlternative(tree, phase.idxInGroup + 1)(phase.prepareForAlternative(tree))
  }

  def goAlternative(tree: Alternative, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxAlternativeTransPhase(start)
    if (phase == null) tree
    else phase.transformAlternative(tree)(ctx) match {
      case tree1: Alternative => goAlternative(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepUnApply(tree: UnApply, start: Int)(implicit ctx: Context): Context = {
    val phase = nxUnApplyPrepPhase(start)
    if (phase == null) ctx
    else prepUnApply(tree, phase.idxInGroup + 1)(phase.prepareForUnApply(tree))
  }

  def goUnApply(tree: UnApply, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxUnApplyTransPhase(start)
    if (phase == null) tree
    else phase.transformUnApply(tree)(ctx) match {
      case tree1: UnApply => goUnApply(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepValDef(tree: ValDef, start: Int)(implicit ctx: Context): Context = {
    val phase = nxValDefPrepPhase(start)
    if (phase == null) ctx
    else prepValDef(tree, phase.idxInGroup + 1)(phase.prepareForValDef(tree))
  }

  def goValDef(tree: ValDef, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxValDefTransPhase(start)
    if (phase == null) tree
    else phase.transformValDef(tree)(ctx) match {
      case tree1: ValDef => goValDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepDefDef(tree: DefDef, start: Int)(implicit ctx: Context): Context = {
    val phase = nxDefDefPrepPhase(start)
    if (phase == null) ctx
    else prepDefDef(tree, phase.idxInGroup + 1)(phase.prepareForDefDef(tree))
  }

  def goDefDef(tree: DefDef, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxDefDefTransPhase(start)
    if (phase == null) tree
    else phase.transformDefDef(tree)(ctx) match {
      case tree1: DefDef => goDefDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTypeDef(tree: TypeDef, start: Int)(implicit ctx: Context): Context = {
    val phase = nxTypeDefPrepPhase(start)
    if (phase == null) ctx
    else prepTypeDef(tree, phase.idxInGroup + 1)(phase.prepareForTypeDef(tree))
  }

  def goTypeDef(tree: TypeDef, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxTypeDefTransPhase(start)
    if (phase == null) tree
    else phase.transformTypeDef(tree)(ctx) match {
      case tree1: TypeDef => goTypeDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepTemplate(tree: Template, start: Int)(implicit ctx: Context): Context = {
    val phase = nxTemplatePrepPhase(start)
    if (phase == null) ctx
    else prepTemplate(tree, phase.idxInGroup + 1)(phase.prepareForTemplate(tree))
  }

  def goTemplate(tree: Template, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxTemplateTransPhase(start)
    if (phase == null) tree
    else phase.transformTemplate(tree)(ctx) match {
      case tree1: Template => goTemplate(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepPackageDef(tree: PackageDef, start: Int)(implicit ctx: Context): Context = {
    val phase = nxPackageDefPrepPhase(start)
    if (phase == null) ctx
    else prepPackageDef(tree, phase.idxInGroup + 1)(phase.prepareForPackageDef(tree))
  }

  def goPackageDef(tree: PackageDef, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxPackageDefTransPhase(start)
    if (phase == null) tree
    else phase.transformPackageDef(tree)(ctx) match {
      case tree1: PackageDef => goPackageDef(tree1, phase.idxInGroup + 1)
      case tree1 => transformNode(tree1, phase.idxInGroup + 1)
    }
  }

  def prepStats(trees: List[Tree], start: Int)(implicit ctx: Context): Context = {
    val phase = nxStatsPrepPhase(start)
    if (phase == null) ctx
    else prepStats(trees, phase.idxInGroup + 1)(phase.prepareForStats(trees))
  }

  def goStats(trees: List[Tree], start: Int)(implicit ctx: Context): List[Tree] = {
    val phase = nxStatsTransPhase(start)
    if (phase == null) trees
    else goStats(phase.transformStats(trees)(ctx), phase.idxInGroup + 1)
  }

  def prepUnit(tree: Tree, start: Int)(implicit ctx: Context): Context = {
    val phase = nxUnitPrepPhase(start)
    if (phase == null) ctx
    else prepUnit(tree, phase.idxInGroup + 1)(phase.prepareForUnit(tree))
  }

  def goUnit(tree: Tree, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxUnitTransPhase(start)
    if (phase == null) tree
    else goUnit(phase.transformUnit(tree)(ctx), phase.idxInGroup + 1)
  }

  def prepOther(tree: Tree, start: Int)(implicit ctx: Context): Context = {
    val phase = nxOtherPrepPhase(start)
    if (phase == null) ctx
    else prepOther(tree, phase.idxInGroup + 1)(phase.prepareForOther(tree))
  }

  def goOther(tree: Tree, start: Int)(implicit ctx: Context): Tree = {
    val phase = nxOtherTransPhase(start)
    if (phase == null) tree
    else goOther(phase.transformOther(tree)(ctx), phase.idxInGroup + 1)
  }
}
