package dotty.tools
package dotc
package transform

import core._
import ast.Trees._
import Contexts._, Phases._, Periods._, Symbols._, Decorators._
import Flags.PackageVal
import scala.annotation.tailrec
import config.Printers.transforms
import scala.util.control.NonFatal
import reporting.trace
import annotation.switch

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
    def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set.empty

    final override def relaxedTyping = superPhase.relaxedTyping

    /** If set, use relaxed typing for all phases in group */
    def relaxedTypingInGroup = false

    val cpy: TypedTreeCopier = cpyBetweenPhases

    def prepareForIdent(tree: Ident)(implicit ctx: Context): Context = ctx
    def prepareForSelect(tree: Select)(implicit ctx: Context): Context = ctx
    def prepareForThis(tree: This)(implicit ctx: Context) = ctx
    def prepareForSuper(tree: Super)(implicit ctx: Context) = ctx
    def prepareForApply(tree: Apply)(implicit ctx: Context) = ctx
    def prepareForTypeApply(tree: TypeApply)(implicit ctx: Context) = ctx
    def prepareForLiteral(tree: Literal)(implicit ctx: Context) = ctx
    def prepareForNew(tree: New)(implicit ctx: Context) = ctx
    def prepareForTyped(tree: Typed)(implicit ctx: Context) = ctx
    def prepareForAssign(tree: Assign)(implicit ctx: Context) = ctx
    def prepareForBlock(tree: Block)(implicit ctx: Context) = ctx
    def prepareForIf(tree: If)(implicit ctx: Context) = ctx
    def prepareForClosure(tree: Closure)(implicit ctx: Context) = ctx
    def prepareForMatch(tree: Match)(implicit ctx: Context) = ctx
    def prepareForCaseDef(tree: CaseDef)(implicit ctx: Context) = ctx
    def prepareForReturn(tree: Return)(implicit ctx: Context) = ctx
    def prepareForTry(tree: Try)(implicit ctx: Context) = ctx
    def prepareForSeqLiteral(tree: SeqLiteral)(implicit ctx: Context) = ctx
    def prepareForInlined(tree: Inlined)(implicit ctx: Context) = ctx
    def prepareForTypeTree(tree: TypeTree)(implicit ctx: Context) = ctx
    def prepareForBind(tree: Bind)(implicit ctx: Context) = ctx
    def prepareForAlternative(tree: Alternative)(implicit ctx: Context) = ctx
    def prepareForUnApply(tree: UnApply)(implicit ctx: Context) = ctx
    def prepareForValDef(tree: ValDef)(implicit ctx: Context) = ctx
    def prepareForDefDef(tree: DefDef)(implicit ctx: Context) = ctx
    def prepareForTypeDef(tree: TypeDef)(implicit ctx: Context) = ctx
    def prepareForTemplate(tree: Template)(implicit ctx: Context) = ctx
    def prepareForPackageDef(tree: PackageDef)(implicit ctx: Context) = ctx
    def prepareForStats(trees: List[Tree])(implicit ctx: Context) = ctx
    def prepareForUnit(tree: Tree)(implicit ctx: Context) = ctx
    def prepareForOther(tree: Tree)(implicit ctx: Context) = ctx

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
    def transformReturn(tree: Return)(implicit ctx: Context): Tree = tree
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

    protected def singletonGroup = new MegaPhase(Array(this))

    override def run(implicit ctx: Context): Unit =
      singletonGroup.run
  }

  private type Transformer[-T, R] = (T, Context) => R
  private type IndexedTransformer[-T, R] = MiniPhase => Transformer[T, R]

  private val idNodeTransformer: Transformer[AnyRef, AnyRef] = (t, ctx) => t
  private val idContextTransformer: Transformer[AnyRef, Context] = (t, ctx) => ctx
}
import MegaPhase._

class MegaPhase(val miniPhases: Array[MiniPhase]) extends Phase {
  import ast.tpd._

  override val phaseName =
    if (miniPhases.length == 1) miniPhases(0).phaseName
    else miniPhases.map(_.phaseName).mkString("MegaPhase{", ", ", "}")

  private var relaxedTypingCache: Boolean = _
  private var relaxedTypingKnown = false

  override final def relaxedTyping = {
    if (!relaxedTypingKnown) {
      relaxedTypingCache = miniPhases.exists(_.relaxedTypingInGroup)
      relaxedTypingKnown = true
    }
    relaxedTypingCache
  }

  private val cpy: TypedTreeCopier = cpyBetweenPhases

  /** Transform node using all phases in this group that have idxInGroup >= start */
  def transformNode(tree: Tree, start: Int)(implicit ctx: Context) =
    nodeTransformer(tree.tag)(start)(tree, ctx)

  /** Transform full tree using all phases in this group that have idxInGroup >= start */
  def transformTree(tree: Tree, start: Int)(implicit ctx: Context): Tree = {
    val tag = tree.tag
    val nestedCtx = contextTransformer(tag)(start)(tree, ctx)
    val transformer = nodeTransformer(tag)(start)

    def trans(tree: Tree)(implicit ctx: Context) = transformer(tree, ctx)

    def localContext(implicit ctx: Context) = {
      val sym = tree.symbol
      val owner = if (sym is PackageVal) sym.moduleClass else sym
      ctx.fresh.setOwner(owner)
    }

    { implicit val ctx = nestedCtx
      (tag: @switch) match {
        case Tag.Select =>
          val tree1 = tree.asInstanceOf[Select]
          val qual = transformTree(tree1.qualifier, start)
          trans(cpy.Select(tree1)(qual, tree1.name))
        case Tag.Super =>
          val tree1 = tree.asInstanceOf[Super]
          val qual = transformTree(tree1.qual, start)
          trans(cpy.Super(tree1)(qual, tree1.mix))
        case Tag.Apply =>
          val tree1 = tree.asInstanceOf[Apply]
          val fun = transformTree(tree1.fun, start)
          val args = transformTrees(tree1.args, start)
          trans(cpy.Apply(tree1)(fun, args))
        case Tag.TypeApply =>
          val tree1 = tree.asInstanceOf[TypeApply]
          val fun = transformTree(tree1.fun, start)
          val args = transformTrees(tree1.args, start)
          trans(cpy.TypeApply(tree1)(fun, args))
        case Tag.New =>
          val tree1 = tree.asInstanceOf[New]
          val tpt = transformTree(tree1.tpt, start)
          trans(cpy.New(tree1)(tpt))
        case Tag.Typed =>
          val tree1 = tree.asInstanceOf[Typed]
          val expr = transformTree(tree1.expr, start)
          val tpt = transformTree(tree1.tpt, start)
          trans(cpy.Typed(tree1)(expr, tpt))
        case Tag.Assign =>
          val tree1 = tree.asInstanceOf[Assign]
          val lhs = transformTree(tree1.lhs, start)
          val rhs = transformTree(tree1.rhs, start)
          trans(cpy.Assign(tree1)(lhs, rhs))
        case Tag.Block =>
          val tree1 = tree.asInstanceOf[Block]
          val stats = transformStats(tree1.stats, ctx.owner, start)
          val expr = transformTree(tree1.expr, start)
          trans(cpy.Block(tree1)(stats, expr))
        case Tag.If =>
          val tree1 = tree.asInstanceOf[If]
          val cond = transformTree(tree1.cond, start)
          val thenp = transformTree(tree1.thenp, start)
          val elsep = transformTree(tree1.elsep, start)
          trans(cpy.If(tree1)(cond, thenp, elsep))
        case Tag.Closure =>
          val tree1 = tree.asInstanceOf[Closure]
          val env = transformTrees(tree1.env, start)
          val meth = transformTree(tree1.meth, start)
          val tpt = transformTree(tree1.tpt, start)
          trans(cpy.Closure(tree1)(env, meth, tpt))
        case Tag.Match =>
          val tree1 = tree.asInstanceOf[Match]
          val selector = transformTree(tree1.selector, start)
          val cases = transformSpecificTrees(tree1.cases, start)
          trans(cpy.Match(tree1)(selector, cases))
        case Tag.CaseDef =>
          val tree1 = tree.asInstanceOf[CaseDef]
          val pat = transformTree(tree1.pat, start)(ctx.addMode(Mode.Pattern))
          val guard = transformTree(tree1.guard, start)
          val body = transformTree(tree1.body, start)
          trans(cpy.CaseDef(tree1)(pat, guard, body))
        case Tag.Return =>
          val tree1 = tree.asInstanceOf[Return]
          val expr = transformTree(tree1.expr, start)
          trans(cpy.Return(tree1)(expr, tree1.from))
            // don't transform `tree1.from`, as this is not a normal ident, but
            // a pointer to the enclosing method.
        case Tag.Try =>
          val tree1 = tree.asInstanceOf[Try]
          val expr = transformTree(tree1.expr, start)
          val cases = transformSpecificTrees(tree1.cases, start)
          val finalizer = transformTree(tree1.finalizer, start)
          trans(cpy.Try(tree1)(expr, cases, finalizer))
        case Tag.SeqLiteral =>
          val tree1 = tree.asInstanceOf[SeqLiteral]
          val elems = transformTrees(tree1.elems, start)
          val elemtpt = transformTree(tree1.elemtpt, start)
          trans(cpy.SeqLiteral(tree1)(elems, elemtpt))
        case Tag.Inlined =>
          val tree1 = tree.asInstanceOf[Inlined]
          val bindings = transformSpecificTrees(tree1.bindings, start)
          val expansion = transformTree(tree1.expansion, start)
          trans(cpy.Inlined(tree1)(tree1.call, bindings, expansion))
        case Tag.Bind =>
          val tree1 = tree.asInstanceOf[Bind]
          val body = transformTree(tree1.body, start)
          trans(cpy.Bind(tree1)(tree1.name, body))
        case Tag.Alternative =>
          val tree1 = tree.asInstanceOf[Alternative]
          val trees = transformTrees(tree1.trees, start)
          trans(cpy.Alternative(tree1)(trees))
        case Tag.UnApply =>
          val tree1 = tree.asInstanceOf[UnApply]
          val fun = transformTree(tree1.fun, start)
          val implicits = transformTrees(tree1.implicits, start)
          val patterns = transformTrees(tree1.patterns, start)
          trans(cpy.UnApply(tree1)(fun, implicits, patterns))
        case Tag.ValDef =>
          val tree1 = tree.asInstanceOf[ValDef]
          def mapValDef(implicit ctx: Context) = {
            val tpt = transformTree(tree1.tpt, start)
            val rhs = transformTree(tree1.rhs, start)
            cpy.ValDef(tree1)(tree1.name, tpt, rhs)
          }
          if (tree1.isEmpty) tree1
          else trans(mapValDef(if (tree.symbol.exists) localContext else ctx))
        case Tag.DefDef =>
          val tree1 = tree.asInstanceOf[DefDef]
          def mapDefDef(implicit ctx: Context) = {
            val tparams = transformSpecificTrees(tree1.tparams, start)
            val vparamss = tree1.vparamss.mapConserve(transformSpecificTrees(_, start))
            val tpt = transformTree(tree1.tpt, start)
            val rhs = transformTree(tree1.rhs, start)
            cpy.DefDef(tree1)(tree1.name, tparams, vparamss, tpt, rhs)
          }
          trans(mapDefDef(localContext))
        case Tag.TypeDef =>
          val tree1 = tree.asInstanceOf[TypeDef]
          val rhs = transformTree(tree1.rhs, start)(localContext)
          trans(cpy.TypeDef(tree1)(tree1.name, rhs))
        case Tag.Template =>
          val tree1 = tree.asInstanceOf[Template]
          val constr = transformSpecificTree(tree1.constr, start)
          val parents = transformTrees(tree1.parents, start)(ctx.superCallContext)
          val self = transformSpecificTree(tree1.self, start)
          val body = transformStats(tree1.body, tree1.symbol, start)
          trans(cpy.Template(tree1)(constr, parents, self, body))
        case Tag.PackageDef =>
          val tree1 = tree.asInstanceOf[PackageDef]
          def mapPackage(implicit ctx: Context) = {
            val pid = transformSpecificTree(tree1.pid, start)
            val stats = transformStats(tree1.stats, tree.symbol, start)
            cpy.PackageDef(tree1)(pid, stats)
          }
          trans(mapPackage(localContext))
        case Tag.Thicket =>
          val tree1 = tree.asInstanceOf[Thicket]
          cpy.Thicket(tree1)(transformTrees(tree1.trees, start))
        case _ =>
          trans(tree)
      }
    }
  }

  def transformSpecificTree[T <: Tree](tree: T, start: Int)(implicit ctx: Context): T =
    transformTree(tree, start).asInstanceOf[T]

  def transformStats(trees: List[Tree], exprOwner: Symbol, start: Int)(implicit ctx: Context): List[Tree] = {
    def transformStat(stat: Tree)(implicit ctx: Context): Tree = stat match {
      case _: Import | _: DefTree => transformTree(stat, start)
      case Thicket(stats) => cpy.Thicket(stat)(stats.mapConserve(transformStat))
      case _ => transformTree(stat, start)(ctx.exprContext(stat, exprOwner))
    }
    val nestedCtx = statsContextTransformer(start)(trees, ctx)
    val newTrees = flatten(trees.mapConserve(transformStat(_)(nestedCtx)))
    statsNodeTransformer(start)(newTrees, nestedCtx)
  }

  def transformUnit(tree: Tree)(implicit ctx: Context) = {
    val nestedCtx = unitContextTransformer(0)(tree, ctx)
    val newTree = transformTree(tree, 0)(nestedCtx)
    unitNodeTransformer(0)(newTree, nestedCtx)
  }

  def transformTrees(trees: List[Tree], start: Int)(implicit ctx: Context): List[Tree] =
    flatten(trees.mapConserve(transformTree(_, start)))

  def transformSpecificTrees[T <: Tree](trees: List[T], start: Int)(implicit ctx: Context): List[T] =
    transformTrees(trees, start).asInstanceOf[List[T]]

  override def run(implicit ctx: Context): Unit =
    ctx.compilationUnit.tpdTree =
      transformUnit(ctx.compilationUnit.tpdTree)(ctx.withPhase(miniPhases.last.next))

  // The rest of this class is all made up by initialization code

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

  /** A transformer array is an array of node or context transformers. It has
   *  one more element than there are miniphases. It is constructed as follows:
   *
   *  - the last element is always `last`, which is the identity transformer
   *    that returns the context or tree unchanged.
   *  - The element corresponding to phase P is `elemFn(P)` if `P` defines a
   *    method with name `methName`, or it the same as the following element
   *    if `P` does not define `methName`.
   */
  private def newTransformerArray[T, R](
      methName: String, elemFn: IndexedTransformer[T, R], last: Transformer[T, R]) = {
    val trans = new Array[Transformer[T, R]](miniPhases.length + 1)
    trans(miniPhases.length) = last
    for (idx <- miniPhases.length - 1 to 0 by -1) {
      val subPhase = miniPhases(idx)
      trans(idx) = if (defines(subPhase, methName)) elemFn(subPhase) else trans(idx + 1)
    }
    trans
  }

  private def newContextTransformerArray[T](suffix: String, elemFn: IndexedTransformer[T, Context]) =
    newTransformerArray[T, Context]("prepareFor" + suffix, elemFn, idContextTransformer.asInstanceOf[Transformer[T, Context]])

  private def newNodeTransformerArray[T, R](suffix: String, elemFn: IndexedTransformer[T, R]) =
    newTransformerArray[T, R]("transform" + suffix, elemFn, idNodeTransformer.asInstanceOf[Transformer[T, R]])

  private val statsContextTransformer = newContextTransformerArray("Stats", prepForStats)
  private val statsNodeTransformer    = newNodeTransformerArray("Stats", transStats)
  private val unitContextTransformer  = newContextTransformerArray("Unit", prepForUnit)
  private val unitNodeTransformer     = newNodeTransformerArray("Unit", transUnit)
  private val otherContextTransformer = newContextTransformerArray("Other", prepForOther)
  private val otherNodeTransformer    = newNodeTransformerArray("Other", transOther)

  private val contextTransformer: Array[Array[Transformer[Tree, Context]]] =
    Array.fill(Tag.NumTags)(otherContextTransformer)
  private val nodeTransformer: Array[Array[Transformer[Tree, Tree]]] =
    Array.fill(Tag.NumTags)(otherNodeTransformer)

  // Dotty problem, replace T with _ in the line below, and you get an irreducible application errpr
  private def init[T](name: String, tag: TreeTag, ctf: IndexedTransformer[T, Context], ntf: IndexedTransformer[T, Tree]): Unit = {
    if (miniPhases.exists(defines(_, "prepareFor" + name)))
      contextTransformer(tag) = newContextTransformerArray(name, ctf.asInstanceOf[IndexedTransformer[Tree, Context]])
    if (miniPhases.exists(defines(_, "transform" + name)))
      nodeTransformer(tag) = newNodeTransformerArray(name, ntf.asInstanceOf[IndexedTransformer[Tree, Tree]])
  }

  init("Ident", Tag.Ident, prepForIdent, transIdent)
  init("Select", Tag.Select, prepForSelect, transSelect)
  init("This", Tag.This, prepForThis, transThis)
  init("Super", Tag.Super, prepForSuper, transSuper)
  init("Apply", Tag.Apply, prepForApply, transApply)
  init("TypeApply", Tag.TypeApply, prepForTypeApply, transTypeApply)
  init("Literal", Tag.Literal, prepForLiteral, transLiteral)
  init("New", Tag.New, prepForNew, transNew)
  init("Typed", Tag.Typed, prepForTyped, transTyped)
  init("Assign", Tag.Assign, prepForAssign, transAssign)
  init("Block", Tag.Block, prepForBlock, transBlock)
  init("If", Tag.If, prepForIf, transIf)
  init("Closure", Tag.Closure, prepForClosure, transClosure)
  init("Match", Tag.Match, prepForMatch, transMatch)
  init("CaseDef", Tag.CaseDef, prepForCaseDef, transCaseDef)
  init("Return", Tag.Return, prepForReturn, transReturn)
  init("Try", Tag.Try, prepForTry, transTry)
  init("SeqLiteral", Tag.SeqLiteral, prepForSeqLiteral, transSeqLiteral)
  init("Inlined", Tag.Inlined, prepForInlined, transInlined)
  init("TypeTree", Tag.TypeTree, prepForTypeTree, transTypeTree)
  init("Bind", Tag.Bind, prepForBind, transBind)
  init("Alternative", Tag.Alternative, prepForAlternative, transAlternative)
  init("UnApply", Tag.UnApply, prepForUnApply, transUnApply)
  init("ValDef", Tag.ValDef, prepForValDef, transValDef)
  init("DefDef", Tag.DefDef, prepForDefDef, transDefDef)
  init("TypeDef", Tag.TypeDef, prepForTypeDef, transTypeDef)
  init("Template", Tag.Template, prepForTemplate, transTemplate)
  init("PackageDef", Tag.PackageDef, prepForPackageDef, transPackageDef)

  private def prepForIdent(phase: MiniPhase): Transformer[Ident, Context] =
    (tree, ctx) => contextTransformer(Tag.Ident)(phase.idxInGroup + 1)(tree, phase.prepareForIdent(tree)(ctx))

  private def prepForSelect(phase: MiniPhase): Transformer[Select, Context] =
    (tree, ctx) => contextTransformer(Tag.Select)(phase.idxInGroup + 1)(tree, phase.prepareForSelect(tree)(ctx))

  private def prepForThis(phase: MiniPhase): Transformer[This, Context] =
    (tree, ctx) => contextTransformer(Tag.This)(phase.idxInGroup + 1)(tree, phase.prepareForThis(tree)(ctx))

  private def prepForSuper(phase: MiniPhase): Transformer[Super, Context] =
    (tree, ctx) => contextTransformer(Tag.Super)(phase.idxInGroup + 1)(tree, phase.prepareForSuper(tree)(ctx))

  private def prepForApply(phase: MiniPhase): Transformer[Apply, Context] =
    (tree, ctx) => contextTransformer(Tag.Apply)(phase.idxInGroup + 1)(tree, phase.prepareForApply(tree)(ctx))

  private def prepForTypeApply(phase: MiniPhase): Transformer[TypeApply, Context] =
    (tree, ctx) => contextTransformer(Tag.TypeApply)(phase.idxInGroup + 1)(tree, phase.prepareForTypeApply(tree)(ctx))

  private def prepForLiteral(phase: MiniPhase): Transformer[Literal, Context] =
    (tree, ctx) => contextTransformer(Tag.Literal)(phase.idxInGroup + 1)(tree, phase.prepareForLiteral(tree)(ctx))

  private def prepForNew(phase: MiniPhase): Transformer[New, Context] =
    (tree, ctx) => contextTransformer(Tag.New)(phase.idxInGroup + 1)(tree, phase.prepareForNew(tree)(ctx))

  private def prepForTyped(phase: MiniPhase): Transformer[Typed, Context] =
    (tree, ctx) => contextTransformer(Tag.Typed)(phase.idxInGroup + 1)(tree, phase.prepareForTyped(tree)(ctx))

  private def prepForAssign(phase: MiniPhase): Transformer[Assign, Context] =
    (tree, ctx) => contextTransformer(Tag.Assign)(phase.idxInGroup + 1)(tree, phase.prepareForAssign(tree)(ctx))

  private def prepForBlock(phase: MiniPhase): Transformer[Block, Context] =
    (tree, ctx) => contextTransformer(Tag.Block)(phase.idxInGroup + 1)(tree, phase.prepareForBlock(tree)(ctx))

  private def prepForIf(phase: MiniPhase): Transformer[If, Context] =
    (tree, ctx) => contextTransformer(Tag.If)(phase.idxInGroup + 1)(tree, phase.prepareForIf(tree)(ctx))

  private def prepForClosure(phase: MiniPhase): Transformer[Closure, Context] =
    (tree, ctx) => contextTransformer(Tag.Closure)(phase.idxInGroup + 1)(tree, phase.prepareForClosure(tree)(ctx))

  private def prepForMatch(phase: MiniPhase): Transformer[Match, Context] =
    (tree, ctx) => contextTransformer(Tag.Match)(phase.idxInGroup + 1)(tree, phase.prepareForMatch(tree)(ctx))

  private def prepForCaseDef(phase: MiniPhase): Transformer[CaseDef, Context] =
    (tree, ctx) => contextTransformer(Tag.CaseDef)(phase.idxInGroup + 1)(tree, phase.prepareForCaseDef(tree)(ctx))

  private def prepForReturn(phase: MiniPhase): Transformer[Return, Context] =
    (tree, ctx) => contextTransformer(Tag.Return)(phase.idxInGroup + 1)(tree, phase.prepareForReturn(tree)(ctx))

  private def prepForTry(phase: MiniPhase): Transformer[Try, Context] =
    (tree, ctx) => contextTransformer(Tag.Try)(phase.idxInGroup + 1)(tree, phase.prepareForTry(tree)(ctx))

  private def prepForSeqLiteral(phase: MiniPhase): Transformer[SeqLiteral, Context] =
    (tree, ctx) => contextTransformer(Tag.SeqLiteral)(phase.idxInGroup + 1)(tree, phase.prepareForSeqLiteral(tree)(ctx))

  private def prepForInlined(phase: MiniPhase): Transformer[Inlined, Context] =
    (tree, ctx) => contextTransformer(Tag.Inlined)(phase.idxInGroup + 1)(tree, phase.prepareForInlined(tree)(ctx))

  private def prepForTypeTree(phase: MiniPhase): Transformer[TypeTree, Context] =
    (tree, ctx) => contextTransformer(Tag.TypeTree)(phase.idxInGroup + 1)(tree, phase.prepareForTypeTree(tree)(ctx))

  private def prepForBind(phase: MiniPhase): Transformer[Bind, Context] =
    (tree, ctx) => contextTransformer(Tag.Bind)(phase.idxInGroup + 1)(tree, phase.prepareForBind(tree)(ctx))

  private def prepForAlternative(phase: MiniPhase): Transformer[Alternative, Context] =
    (tree, ctx) => contextTransformer(Tag.Alternative)(phase.idxInGroup + 1)(tree, phase.prepareForAlternative(tree)(ctx))

  private def prepForUnApply(phase: MiniPhase): Transformer[UnApply, Context] =
    (tree, ctx) => contextTransformer(Tag.UnApply)(phase.idxInGroup + 1)(tree, phase.prepareForUnApply(tree)(ctx))

  private def prepForValDef(phase: MiniPhase): Transformer[ValDef, Context] =
    (tree, ctx) => contextTransformer(Tag.ValDef)(phase.idxInGroup + 1)(tree, phase.prepareForValDef(tree)(ctx))

  private def prepForDefDef(phase: MiniPhase): Transformer[DefDef, Context] =
    (tree, ctx) => contextTransformer(Tag.DefDef)(phase.idxInGroup + 1)(tree, phase.prepareForDefDef(tree)(ctx))

  private def prepForTypeDef(phase: MiniPhase): Transformer[TypeDef, Context] =
    (tree, ctx) => contextTransformer(Tag.TypeDef)(phase.idxInGroup + 1)(tree, phase.prepareForTypeDef(tree)(ctx))

  private def prepForTemplate(phase: MiniPhase): Transformer[Template, Context] =
    (tree, ctx) => contextTransformer(Tag.Template)(phase.idxInGroup + 1)(tree, phase.prepareForTemplate(tree)(ctx))

  private def prepForPackageDef(phase: MiniPhase): Transformer[PackageDef, Context] =
    (tree, ctx) => contextTransformer(Tag.PackageDef)(phase.idxInGroup + 1)(tree, phase.prepareForPackageDef(tree)(ctx))

  private def prepForStats(phase: MiniPhase): Transformer[List[Tree], Context] =
    (trees, ctx) => statsContextTransformer(phase.idxInGroup + 1)(trees, phase.prepareForStats(trees)(ctx))

  private def prepForUnit(phase: MiniPhase): Transformer[Tree, Context] =
    (tree, ctx) => unitContextTransformer(phase.idxInGroup + 1)(tree, phase.prepareForUnit(tree)(ctx))

  private def prepForOther(phase: MiniPhase): Transformer[Tree, Context] =
    (tree, ctx) => otherContextTransformer(phase.idxInGroup + 1)(tree, phase.prepareForOther(tree)(ctx))

  private def transIdent(phase: MiniPhase): Transformer[Ident, Tree] =
    (tree, ctx) => transformNode(phase.transformIdent(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transSelect(phase: MiniPhase): Transformer[Select, Tree] =
    (tree, ctx) => transformNode(phase.transformSelect(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transThis(phase: MiniPhase): Transformer[This, Tree] =
    (tree, ctx) => transformNode(phase.transformThis(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transSuper(phase: MiniPhase): Transformer[Super, Tree] =
    (tree, ctx) => transformNode(phase.transformSuper(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transApply(phase: MiniPhase): Transformer[Apply, Tree] =
    (tree, ctx) => transformNode(phase.transformApply(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transTypeApply(phase: MiniPhase): Transformer[TypeApply, Tree] =
    (tree, ctx) => transformNode(phase.transformTypeApply(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transLiteral(phase: MiniPhase): Transformer[Literal, Tree] =
    (tree, ctx) => transformNode(phase.transformLiteral(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transNew(phase: MiniPhase): Transformer[New, Tree] =
    (tree, ctx) => transformNode(phase.transformNew(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transTyped(phase: MiniPhase): Transformer[Typed, Tree] =
    (tree, ctx) => transformNode(phase.transformTyped(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transAssign(phase: MiniPhase): Transformer[Assign, Tree] =
    (tree, ctx) => transformNode(phase.transformAssign(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transBlock(phase: MiniPhase): Transformer[Block, Tree] =
    (tree, ctx) => transformNode(phase.transformBlock(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transIf(phase: MiniPhase): Transformer[If, Tree] =
    (tree, ctx) => transformNode(phase.transformIf(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transClosure(phase: MiniPhase): Transformer[Closure, Tree] =
    (tree, ctx) => transformNode(phase.transformClosure(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transMatch(phase: MiniPhase): Transformer[Match, Tree] =
    (tree, ctx) => transformNode(phase.transformMatch(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transCaseDef(phase: MiniPhase): Transformer[CaseDef, Tree] =
    (tree, ctx) => transformNode(phase.transformCaseDef(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transReturn(phase: MiniPhase): Transformer[Return, Tree] =
    (tree, ctx) => transformNode(phase.transformReturn(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transTry(phase: MiniPhase): Transformer[Try, Tree] =
    (tree, ctx) => transformNode(phase.transformTry(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transSeqLiteral(phase: MiniPhase): Transformer[SeqLiteral, Tree] =
    (tree, ctx) => transformNode(phase.transformSeqLiteral(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transInlined(phase: MiniPhase): Transformer[Inlined, Tree] =
    (tree, ctx) => transformNode(phase.transformInlined(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transTypeTree(phase: MiniPhase): Transformer[TypeTree, Tree] =
    (tree, ctx) => transformNode(phase.transformTypeTree(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transBind(phase: MiniPhase): Transformer[Bind, Tree] =
    (tree, ctx) => transformNode(phase.transformBind(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transAlternative(phase: MiniPhase): Transformer[Alternative, Tree] =
    (tree, ctx) => transformNode(phase.transformAlternative(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transUnApply(phase: MiniPhase): Transformer[UnApply, Tree] =
    (tree, ctx) => transformNode(phase.transformUnApply(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transValDef(phase: MiniPhase): Transformer[ValDef, Tree] =
    (tree, ctx) => transformNode(phase.transformValDef(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transDefDef(phase: MiniPhase): Transformer[DefDef, Tree] =
    (tree, ctx) => transformNode(phase.transformDefDef(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transTypeDef(phase: MiniPhase): Transformer[TypeDef, Tree] =
    (tree, ctx) => transformNode(phase.transformTypeDef(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transTemplate(phase: MiniPhase): Transformer[Template, Tree] =
    (tree, ctx) => transformNode(phase.transformTemplate(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transPackageDef(phase: MiniPhase): Transformer[PackageDef, Tree] =
    (tree, ctx) => transformNode(phase.transformPackageDef(tree)(ctx), phase.idxInGroup + 1)(ctx)

  private def transStats(phase: MiniPhase): Transformer[List[Tree], List[Tree]] =
    (trees, ctx) => statsNodeTransformer(phase.idxInGroup + 1)(phase.transformStats(trees)(ctx), ctx)

  private def transUnit(phase: MiniPhase): Transformer[Tree, Tree] =
    (tree, ctx) => unitNodeTransformer(phase.idxInGroup + 1)(phase.transformUnit(tree)(ctx), ctx)

  private def transOther(phase: MiniPhase): Transformer[Tree, Tree] =
    (tree, ctx) => otherNodeTransformer(phase.idxInGroup + 1)(phase.transformOther(tree)(ctx), ctx)
}