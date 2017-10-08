package dotty.tools
package dotc
package transform

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.{InfoTransformer, DenotTransformer}
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Flags.PackageVal
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.util.DotClass
import scala.annotation.tailrec
import config.Printers.transforms
import scala.util.control.NonFatal

object TreeTransforms {
  import tpd._

  /** The base class of tree transforms. For each kind of tree K, there are
   *  two methods which can be overridden:
   *
   *  prepareForK // return a new TreeTransform which gets applied to the K
   *  // node and its children
   *  transformK // transform node of type K
   *
   *  If a transform does not need to visit a node or any of its children, it
   *  signals this fact by returning a NoTransform from a prepare method.
   *
   *  If all transforms in a group are NoTransforms, the tree is no longer traversed.
   *
   *
   *            Performance analysis: Taking the dotty compiler frontend as a use case, we are aiming for a warm performance of
   *            about 4000 lines / sec. This means 6 seconds for a codebase of 24'000 lines. Of these the frontend consumes
   *            over 2.5 seconds, erasure and code generation will most likely consume over 1 second each. So we would have
   *            about 1 sec for all other transformations in our budget. Of this second, let's assume a maximum of 20% for
   *            the general dispatch overhead as opposed to the concrete work done in transformations. So that leaves us with
   *            0.2sec, or roughly 600M processor cycles.
   *
   *            Now, to the amount of work that needs to be done. The codebase produces an average of about 250'000 trees after typechecking.
   *            Transformations are likely to make this bigger so let's assume 300K trees on average. We estimate to have about 100
   *            micro-transformations. Let's say 5 transformation groups of 20 micro-transformations each. (by comparison,
   *            scalac has in excess of 20 phases, and most phases do multiple transformations). There are then 30M visits
   *            of a node by a transformation. Each visit has a budget of 20 processor cycles.
   *
   *            A more detailed breakdown: I assume that about one third of all transformations have real work to do for each node.
   *            This might look high, but keep in mind that the most common nodes are Idents and Selects, and most transformations
   *            touch these. By contrast the amount of work for generating new transformations should be negligible.
   *
   *            So, in 400 clock cycles we need to (1) perform a pattern match according to the type of node, (2) generate new
   *            transformations if applicable, (3) reconstitute the tree node from the result of transforming the children, and
   *            (4) chain 7 out of 20 transformations over the resulting tree node. I believe the current algorithm is suitable
   *            for achieving this goal, but there can be no wasted cycles anywhere.
   */
  abstract class TreeTransform extends DotClass {

    def phase: MiniPhase

    def treeTransformPhase: Phase = phase.next

    val cpy: TypedTreeCopier = cpyBetweenPhases

    def prepareForIdent(tree: Ident)(implicit ctx: Context) = this
    def prepareForSelect(tree: Select)(implicit ctx: Context) = this
    def prepareForThis(tree: This)(implicit ctx: Context) = this
    def prepareForSuper(tree: Super)(implicit ctx: Context) = this
    def prepareForApply(tree: Apply)(implicit ctx: Context) = this
    def prepareForTypeApply(tree: TypeApply)(implicit ctx: Context) = this
    def prepareForLiteral(tree: Literal)(implicit ctx: Context) = this
    def prepareForNew(tree: New)(implicit ctx: Context) = this
    def prepareForTyped(tree: Typed)(implicit ctx: Context) = this
    def prepareForAssign(tree: Assign)(implicit ctx: Context) = this
    def prepareForBlock(tree: Block)(implicit ctx: Context) = this
    def prepareForIf(tree: If)(implicit ctx: Context) = this
    def prepareForClosure(tree: Closure)(implicit ctx: Context) = this
    def prepareForMatch(tree: Match)(implicit ctx: Context) = this
    def prepareForCaseDef(tree: CaseDef)(implicit ctx: Context) = this
    def prepareForReturn(tree: Return)(implicit ctx: Context) = this
    def prepareForTry(tree: Try)(implicit ctx: Context) = this
    def prepareForSeqLiteral(tree: SeqLiteral)(implicit ctx: Context) = this
    def prepareForInlined(tree: Inlined)(implicit ctx: Context) = this
    def prepareForTypeTree(tree: TypeTree)(implicit ctx: Context) = this
    def prepareForBind(tree: Bind)(implicit ctx: Context) = this
    def prepareForAlternative(tree: Alternative)(implicit ctx: Context) = this
    def prepareForTypeDef(tree: TypeDef)(implicit ctx: Context) = this
    def prepareForUnApply(tree: UnApply)(implicit ctx: Context) = this
    def prepareForValDef(tree: ValDef)(implicit ctx: Context) = this
    def prepareForDefDef(tree: DefDef)(implicit ctx: Context) = this
    def prepareForTemplate(tree: Template)(implicit ctx: Context) = this
    def prepareForPackageDef(tree: PackageDef)(implicit ctx: Context) = this
    def prepareForStats(trees: List[Tree])(implicit ctx: Context) = this

    def prepareForUnit(tree: Tree)(implicit ctx: Context) = this

    def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformThis(tree: This)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformSuper(tree: Super)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformLiteral(tree: Literal)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformNew(tree: New)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformTyped(tree: Typed)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformIf(tree: If)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformClosure(tree: Closure)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformMatch(tree: Match)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformCaseDef(tree: CaseDef)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformReturn(tree: Return)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformTry(tree: Try)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformInlined(tree: Inlined)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformBind(tree: Bind)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformAlternative(tree: Alternative)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformUnApply(tree: UnApply)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformTypeDef(tree: TypeDef)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformPackageDef(tree: PackageDef)(implicit ctx: Context, info: TransformerInfo): Tree = tree
    def transformStats(trees: List[Tree])(implicit ctx: Context, info: TransformerInfo): List[Tree] = trees
    def transformOther(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = tree

    def transformUnit(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = tree

    /** Transform tree using all transforms of current group (including this one) */
    def transform(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = info.group.transform(tree, info, 0)

    /** Transform subtree using all transforms following the current one in this group */
    def transformFollowingDeep(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = info.group.transform(tree, info, phase.idx + 1)

    /** Transform single node using all transforms following the current one in this group */
    def transformFollowing(tree: Tree)(implicit ctx: Context, info: TransformerInfo): Tree = info.group.transformSingle(tree, phase.idx + 1)

    def atGroupEnd[T](action : Context => T)(implicit ctx: Context, info: TransformerInfo) = {
      val last = info.transformers(info.transformers.length - 1)
      action(ctx.withPhase(last.phase.next))
    }
  }

  /** A phase that defines a TreeTransform to be used in a group */
  trait MiniPhase extends Phase { thisPhase =>
    def treeTransform: TreeTransform

    /** id of this mini phase in group */
    var idx: Int = _

    /** List of names of phases that should have finished their processing of all compilation units
     *  before this phase starts
     */
    def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set.empty

    protected def mkTreeTransformer = new TreeTransformer {
      override def phaseName: String = thisPhase.phaseName
      override def miniPhases = Array(thisPhase)
    }

    override def run(implicit ctx: Context): Unit = {
      mkTreeTransformer.run
    }
  }

  /** A mini phase that is its own tree transform */
  abstract class MiniPhaseTransform extends TreeTransform with MiniPhase {
    def treeTransform = this
    def phase = this
 }

  /** A helper trait to transform annotations on MemberDefs */
  trait AnnotationTransformer extends MiniPhaseTransform with DenotTransformer {

    val annotationTransformer = mkTreeTransformer
    override final def treeTransformPhase = this
      // need to run at own phase because otherwise we get ahead of ourselves in transforming denotations

    abstract override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
      super.transform(ref) match {
        case ref1: SymDenotation if ref1.symbol.isDefinedInCurrentRun =>
          val annots = ref1.annotations
          val annotTrees = annots.map(_.tree)
          val annotTrees1 = annotTrees.mapConserve(annotationTransformer.macroTransform)
          if (annotTrees eq annotTrees1) ref1
          else {
            val derivedAnnots = (annots, annotTrees1).zipped.map((annot, annotTree1) =>
              annot.derivedAnnotation(annotTree1))
            ref1.copySymDenotation(annotations = derivedAnnots).copyCaches(ref1, ctx.phase.next)
          }
        case ref1 =>
          ref1
      }
  }

  private class NoTreeTransform extends TreeTransform {
    def phase = unsupported("phase")
  }

  @sharable val NoTransform: TreeTransform = new NoTreeTransform

  type Mutator[T] = (TreeTransform, T, Context) => TreeTransform

  class TransformerInfo(val transformers: Array[TreeTransform], val nx: NXTransformations, val group: TreeTransformer)

  /** This class maintains track of which methods are redefined in MiniPhases and creates execution plans for transformXXX and prepareXXX
   *  Thanks to Martin for this idea
   *  @see NXTransformations.index for format of plan
   */
  class NXTransformations {
    private val clsMethodsCache = new java.util.IdentityHashMap[Class[_], Array[java.lang.reflect.Method]]

    // TODO: We spend too much time here. See if we can call it less or make it faster,
    // e.g. by checking `cls.getMethod(name, ...).getDeclaringClass != classOf[TreeTransform]` instead.
    private def hasRedefinedMethod(cls: Class[_], name: String): Boolean = {
      if (cls.eq(classOf[TreeTransform]) || cls.eq(classOf[NoTreeTransform]) ||
          cls.eq(classOf[MiniPhaseTransform]))
        return false

      // Class#getDeclaredMethods is slow, so we cache its output
      var clsMethods = clsMethodsCache.get(cls)
      if (clsMethods eq null) {
        clsMethods = cls.getDeclaredMethods
        clsMethodsCache.put(cls, clsMethods)
      }

      var i = clsMethods.length - 1
      while (i >= 0) {
        if (clsMethods(i).getName == name)
          return true
        i -= 1
      }

      hasRedefinedMethod(cls.getSuperclass, name)
    }

    /** Create an index array `next` of size one larger than the size of `transforms` such that
     *  for each index i, `next(i)` is the smallest index j such that
     *
     *  i <= j
     *  j == transforms.length || transform(j) defines a non-default method with given `name`
     */
    private def index(transformations: Array[Class[_]], name: String): Array[Int] = {
      val len = transformations.length
      val next = new Array[Int](len + 1)
      var nextTransform: Int = len

      /* loop invariant: nextTransform == the smallest j such that
       * i < j and
       * j == transforms.length || transform(j) defines a non-default method with given `name`
       */
      next(len) = len
      var i = len - 1
      while (i >= 0) {
        // update nextTransform if this phase redefines the method
        if (hasRedefinedMethod(transformations(i), name)) {
          nextTransform = i
        }
        next(i) = nextTransform
        i -= 1
      }
      next
    }

    private def indexUpdate(prev: Array[Int], changedTransformation: Class[_], index: Int, name: String, copy: Boolean = true) = {
      val isDefinedNow = hasRedefinedMethod(changedTransformation, name)
      val wasDefinedBefore = prev(index) == index
      if (isDefinedNow == wasDefinedBefore) prev
      else {
        val result = if (copy) prev.clone() else prev
        val oldValue = result(index)
        val newValue =
          if (wasDefinedBefore /* && !isDefinedNow */ ) prev(index + 1)
          else index // isDefinedNow
        var i = index
        while (i >= 0 && result(i) == oldValue) {
          result(i) = newValue
          i -= 1
        }
        result
      }
    }

    def this(transformations: Array[Class[_]]) = {
      this()
      nxPrepIdent = index(transformations, "prepareForIdent")
      nxPrepSelect = index(transformations, "prepareForSelect")
      nxPrepThis = index(transformations, "prepareForThis")
      nxPrepSuper = index(transformations, "prepareForSuper")
      nxPrepApply = index(transformations, "prepareForApply")
      nxPrepTypeApply = index(transformations, "prepareForTypeApply")
      nxPrepLiteral = index(transformations, "prepareForLiteral")
      nxPrepNew = index(transformations, "prepareForNew")
      nxPrepTyped = index(transformations, "prepareForTyped")
      nxPrepAssign = index(transformations, "prepareForAssign")
      nxPrepBlock = index(transformations, "prepareForBlock")
      nxPrepIf = index(transformations, "prepareForIf")
      nxPrepClosure = index(transformations, "prepareForClosure")
      nxPrepCaseDef = index(transformations, "prepareForCaseDef")
      nxPrepMatch = index(transformations, "prepareForMatch")
      nxPrepReturn = index(transformations, "prepareForReturn")
      nxPrepTry = index(transformations, "prepareForTry")
      nxPrepSeqLiteral = index(transformations, "prepareForSeqLiteral")
      nxPrepInlined = index(transformations, "prepareForInlined")
      nxPrepTypeTree = index(transformations, "prepareForTypeTree")
      nxPrepBind = index(transformations, "prepareForBind")
      nxPrepAlternative = index(transformations, "prepareForAlternative")
      nxPrepUnApply = index(transformations, "prepareForUnApply")
      nxPrepValDef = index(transformations, "prepareForValDef")
      nxPrepDefDef = index(transformations, "prepareForDefDef")
      nxPrepTypeDef = index(transformations, "prepareForTypeDef")
      nxPrepTemplate = index(transformations, "prepareForTemplate")
      nxPrepPackageDef = index(transformations, "prepareForPackageDef")
      nxPrepStats = index(transformations, "prepareForStats")
      nxPrepUnit = index(transformations, "prepareForUnit")

      nxTransIdent = index(transformations, "transformIdent")
      nxTransSelect = index(transformations, "transformSelect")
      nxTransThis = index(transformations, "transformThis")
      nxTransSuper = index(transformations, "transformSuper")
      nxTransApply = index(transformations, "transformApply")
      nxTransTypeApply = index(transformations, "transformTypeApply")
      nxTransLiteral = index(transformations, "transformLiteral")
      nxTransNew = index(transformations, "transformNew")
      nxTransTyped = index(transformations, "transformTyped")
      nxTransAssign = index(transformations, "transformAssign")
      nxTransBlock = index(transformations, "transformBlock")
      nxTransIf = index(transformations, "transformIf")
      nxTransClosure = index(transformations, "transformClosure")
      nxTransMatch = index(transformations, "transformMatch")
      nxTransCaseDef = index(transformations, "transformCaseDef")
      nxTransReturn = index(transformations, "transformReturn")
      nxTransTry = index(transformations, "transformTry")
      nxTransSeqLiteral = index(transformations, "transformSeqLiteral")
      nxTransInlined = index(transformations, "transformInlined")
      nxTransTypeTree = index(transformations, "transformTypeTree")
      nxTransBind = index(transformations, "transformBind")
      nxTransAlternative = index(transformations, "transformAlternative")
      nxTransUnApply = index(transformations, "transformUnApply")
      nxTransValDef = index(transformations, "transformValDef")
      nxTransDefDef = index(transformations, "transformDefDef")
      nxTransTypeDef = index(transformations, "transformTypeDef")
      nxTransTemplate = index(transformations, "transformTemplate")
      nxTransPackageDef = index(transformations, "transformPackageDef")
      nxTransStats = index(transformations, "transformStats")
      nxTransUnit = index(transformations, "transformUnit")
      nxTransOther = index(transformations, "transformOther")
    }

    def this(transformations: Array[TreeTransform]) = {
      this(transformations.map(_.getClass).asInstanceOf[Array[Class[_]]])
    }

    def this(prev: NXTransformations, changedTransformation: TreeTransform, transformationIndex: Int, reuse: Boolean = false) = {
      this()
      val copy = !reuse
      val changedTransformationClass = changedTransformation.getClass
      nxPrepIdent = indexUpdate(prev.nxPrepIdent, changedTransformationClass, transformationIndex, "prepareForIdent", copy)
      nxPrepSelect = indexUpdate(prev.nxPrepSelect, changedTransformationClass, transformationIndex, "prepareForSelect", copy)
      nxPrepThis = indexUpdate(prev.nxPrepThis, changedTransformationClass, transformationIndex, "prepareForThis", copy)
      nxPrepSuper = indexUpdate(prev.nxPrepSuper, changedTransformationClass, transformationIndex, "prepareForSuper", copy)
      nxPrepApply = indexUpdate(prev.nxPrepApply, changedTransformationClass, transformationIndex, "prepareForApply", copy)
      nxPrepTypeApply = indexUpdate(prev.nxPrepTypeApply, changedTransformationClass, transformationIndex, "prepareForTypeApply", copy)
      nxPrepLiteral = indexUpdate(prev.nxPrepLiteral, changedTransformationClass, transformationIndex, "prepareForLiteral", copy)
      nxPrepNew = indexUpdate(prev.nxPrepNew, changedTransformationClass, transformationIndex, "prepareForNew", copy)
      nxPrepTyped = indexUpdate(prev.nxPrepTyped, changedTransformationClass, transformationIndex, "prepareForTyped", copy)
      nxPrepAssign = indexUpdate(prev.nxPrepAssign, changedTransformationClass, transformationIndex, "prepareForAssign", copy)
      nxPrepBlock = indexUpdate(prev.nxPrepBlock, changedTransformationClass, transformationIndex, "prepareForBlock", copy)
      nxPrepIf = indexUpdate(prev.nxPrepIf, changedTransformationClass, transformationIndex, "prepareForIf", copy)
      nxPrepClosure = indexUpdate(prev.nxPrepClosure, changedTransformationClass, transformationIndex, "prepareForClosure", copy)
      nxPrepMatch = indexUpdate(prev.nxPrepMatch, changedTransformationClass, transformationIndex, "prepareForMatch", copy)
      nxPrepCaseDef = indexUpdate(prev.nxPrepCaseDef, changedTransformationClass, transformationIndex, "prepareForCaseDef", copy)
      nxPrepReturn = indexUpdate(prev.nxPrepReturn, changedTransformationClass, transformationIndex, "prepareForReturn", copy)
      nxPrepTry = indexUpdate(prev.nxPrepTry, changedTransformationClass, transformationIndex, "prepareForTry", copy)
      nxPrepSeqLiteral = indexUpdate(prev.nxPrepSeqLiteral, changedTransformationClass, transformationIndex, "prepareForSeqLiteral", copy)
      nxPrepInlined = indexUpdate(prev.nxPrepInlined, changedTransformationClass, transformationIndex, "prepareForInlined", copy)
      nxPrepTypeTree = indexUpdate(prev.nxPrepTypeTree, changedTransformationClass, transformationIndex, "prepareForTypeTree", copy)
      nxPrepBind = indexUpdate(prev.nxPrepBind, changedTransformationClass, transformationIndex, "prepareForBind", copy)
      nxPrepAlternative = indexUpdate(prev.nxPrepAlternative, changedTransformationClass, transformationIndex, "prepareForAlternative", copy)
      nxPrepUnApply = indexUpdate(prev.nxPrepUnApply, changedTransformationClass, transformationIndex, "prepareForUnApply", copy)
      nxPrepValDef = indexUpdate(prev.nxPrepValDef, changedTransformationClass, transformationIndex, "prepareForValDef", copy)
      nxPrepDefDef = indexUpdate(prev.nxPrepDefDef, changedTransformationClass, transformationIndex, "prepareForDefDef", copy)
      nxPrepTypeDef = indexUpdate(prev.nxPrepTypeDef, changedTransformationClass, transformationIndex, "prepareForTypeDef", copy)
      nxPrepTemplate = indexUpdate(prev.nxPrepTemplate, changedTransformationClass, transformationIndex, "prepareForTemplate", copy)
      nxPrepPackageDef = indexUpdate(prev.nxPrepPackageDef, changedTransformationClass, transformationIndex, "prepareForPackageDef", copy)
      nxPrepStats = indexUpdate(prev.nxPrepStats, changedTransformationClass, transformationIndex, "prepareForStats", copy)

      nxTransIdent = indexUpdate(prev.nxTransIdent, changedTransformationClass, transformationIndex, "transformIdent", copy)
      nxTransSelect = indexUpdate(prev.nxTransSelect, changedTransformationClass, transformationIndex, "transformSelect", copy)
      nxTransThis = indexUpdate(prev.nxTransThis, changedTransformationClass, transformationIndex, "transformThis", copy)
      nxTransSuper = indexUpdate(prev.nxTransSuper, changedTransformationClass, transformationIndex, "transformSuper", copy)
      nxTransApply = indexUpdate(prev.nxTransApply, changedTransformationClass, transformationIndex, "transformApply", copy)
      nxTransTypeApply = indexUpdate(prev.nxTransTypeApply, changedTransformationClass, transformationIndex, "transformTypeApply", copy)
      nxTransLiteral = indexUpdate(prev.nxTransLiteral, changedTransformationClass, transformationIndex, "transformLiteral", copy)
      nxTransNew = indexUpdate(prev.nxTransNew, changedTransformationClass, transformationIndex, "transformNew", copy)
      nxTransTyped = indexUpdate(prev.nxTransTyped, changedTransformationClass, transformationIndex, "transformTyped", copy)
      nxTransAssign = indexUpdate(prev.nxTransAssign, changedTransformationClass, transformationIndex, "transformAssign", copy)
      nxTransBlock = indexUpdate(prev.nxTransBlock, changedTransformationClass, transformationIndex, "transformBlock", copy)
      nxTransIf = indexUpdate(prev.nxTransIf, changedTransformationClass, transformationIndex, "transformIf", copy)
      nxTransClosure = indexUpdate(prev.nxTransClosure, changedTransformationClass, transformationIndex, "transformClosure", copy)
      nxTransMatch = indexUpdate(prev.nxTransMatch, changedTransformationClass, transformationIndex, "transformMatch", copy)
      nxTransCaseDef = indexUpdate(prev.nxTransCaseDef, changedTransformationClass, transformationIndex, "transformCaseDef", copy)
      nxTransReturn = indexUpdate(prev.nxTransReturn, changedTransformationClass, transformationIndex, "transformReturn", copy)
      nxTransTry = indexUpdate(prev.nxTransTry, changedTransformationClass, transformationIndex, "transformTry", copy)
      nxTransSeqLiteral = indexUpdate(prev.nxTransSeqLiteral, changedTransformationClass, transformationIndex, "transformSeqLiteral", copy)
      nxTransInlined = indexUpdate(prev.nxTransInlined, changedTransformationClass, transformationIndex, "transformInlined", copy)
      nxTransTypeTree = indexUpdate(prev.nxTransTypeTree, changedTransformationClass, transformationIndex, "transformTypeTree", copy)
      nxTransBind = indexUpdate(prev.nxTransBind, changedTransformationClass, transformationIndex, "transformBind", copy)
      nxTransAlternative = indexUpdate(prev.nxTransAlternative, changedTransformationClass, transformationIndex, "transformAlternative", copy)
      nxTransUnApply = indexUpdate(prev.nxTransUnApply, changedTransformationClass, transformationIndex, "transformUnApply", copy)
      nxTransValDef = indexUpdate(prev.nxTransValDef, changedTransformationClass, transformationIndex, "transformValDef", copy)
      nxTransDefDef = indexUpdate(prev.nxTransDefDef, changedTransformationClass, transformationIndex, "transformDefDef", copy)
      nxTransTypeDef = indexUpdate(prev.nxTransTypeDef, changedTransformationClass, transformationIndex, "transformTypeDef", copy)
      nxTransTemplate = indexUpdate(prev.nxTransTemplate, changedTransformationClass, transformationIndex, "transformTemplate", copy)
      nxTransPackageDef = indexUpdate(prev.nxTransPackageDef, changedTransformationClass, transformationIndex, "transformPackageDef", copy)
      nxTransStats = indexUpdate(prev.nxTransStats, changedTransformationClass, transformationIndex, "transformStats", copy)
      nxTransOther = indexUpdate(prev.nxTransOther, changedTransformationClass, transformationIndex, "transformOther", copy)
    }

    /** Those arrays are used as "execution plan" in order to only execute non-trivial transformations\preparations
     *  for every integer i array(i) contains first non trivial transformation\preparation on particular tree subtype.
     *  If no nontrivial transformation are left stored value is greater than  transformers.size
     */
    var nxPrepIdent: Array[Int] = _
    var nxPrepSelect: Array[Int] = _
    var nxPrepThis: Array[Int] = _
    var nxPrepSuper: Array[Int] = _
    var nxPrepApply: Array[Int] = _
    var nxPrepTypeApply: Array[Int] = _
    var nxPrepLiteral: Array[Int] = _
    var nxPrepNew: Array[Int] = _
    var nxPrepTyped: Array[Int] = _
    var nxPrepAssign: Array[Int] = _
    var nxPrepBlock: Array[Int] = _
    var nxPrepIf: Array[Int] = _
    var nxPrepClosure: Array[Int] = _
    var nxPrepMatch: Array[Int] = _
    var nxPrepCaseDef: Array[Int] = _
    var nxPrepReturn: Array[Int] = _
    var nxPrepTry: Array[Int] = _
    var nxPrepSeqLiteral: Array[Int] = _
    var nxPrepInlined: Array[Int] = _
    var nxPrepTypeTree: Array[Int] = _
    var nxPrepBind: Array[Int] = _
    var nxPrepAlternative: Array[Int] = _
    var nxPrepUnApply: Array[Int] = _
    var nxPrepValDef: Array[Int] = _
    var nxPrepDefDef: Array[Int] = _
    var nxPrepTypeDef: Array[Int] = _
    var nxPrepTemplate: Array[Int] = _
    var nxPrepPackageDef: Array[Int] = _
    var nxPrepStats: Array[Int] = _
    var nxPrepUnit: Array[Int] = _

    var nxTransIdent: Array[Int] = _
    var nxTransSelect: Array[Int] = _
    var nxTransThis: Array[Int] = _
    var nxTransSuper: Array[Int] = _
    var nxTransApply: Array[Int] = _
    var nxTransTypeApply: Array[Int] = _
    var nxTransLiteral: Array[Int] = _
    var nxTransNew: Array[Int] = _
    var nxTransTyped: Array[Int] = _
    var nxTransAssign: Array[Int] = _
    var nxTransBlock: Array[Int] = _
    var nxTransIf: Array[Int] = _
    var nxTransClosure: Array[Int] = _
    var nxTransMatch: Array[Int] = _
    var nxTransCaseDef: Array[Int] = _
    var nxTransReturn: Array[Int] = _
    var nxTransTry: Array[Int] = _
    var nxTransSeqLiteral: Array[Int] = _
    var nxTransInlined: Array[Int] = _
    var nxTransTypeTree: Array[Int] = _
    var nxTransBind: Array[Int] = _
    var nxTransAlternative: Array[Int] = _
    var nxTransUnApply: Array[Int] = _
    var nxTransValDef: Array[Int] = _
    var nxTransDefDef: Array[Int] = _
    var nxTransTypeDef: Array[Int] = _
    var nxTransTemplate: Array[Int] = _
    var nxTransPackageDef: Array[Int] = _
    var nxTransStats: Array[Int] = _
    var nxTransUnit: Array[Int] = _
    var nxTransOther: Array[Int] = _
  }

  /** A group of tree transforms that are applied in sequence during the same phase */
  abstract class TreeTransformer extends Phase {

    def miniPhases: Array[MiniPhase]

    override def run(implicit ctx: Context): Unit = {
      val curTree = ctx.compilationUnit.tpdTree
      val newTree = macroTransform(curTree)
      ctx.compilationUnit.tpdTree = newTree
    }

    def mutateTransformers[T](info: TransformerInfo, mutator: Mutator[T], mutationPlan: Array[Int], tree: T, cur: Int)(implicit ctx: Context) = {
      var transformersCopied = false
      var nxCopied = false
      var result = info.transformers
      var resultNX = info.nx
      var i = mutationPlan(cur)
        // @DarkDimius You commented on the previous version
        //
        //     var i = mutationPlan(0) // if TreeTransform.transform() method didn't exist we could have used mutationPlan(cur)
        //
        // But we need to use `cur` or otherwise we call prepare actions preceding the
        // phase that issued a transformFollowing. This can lead to "denotation not defined
        // here" errors. Note that tests still pass with the current modified code.
      val l = result.length
      var allDone = i < l
      while (i < l) {
        val oldTransform = result(i)
        val newTransform = mutator(oldTransform, tree, ctx.withPhase(oldTransform.treeTransformPhase))
        allDone = allDone && (newTransform eq NoTransform)
        if (!(oldTransform eq newTransform)) {
          if (!transformersCopied) result = result.clone()
          transformersCopied = true
          result(i) = newTransform
          if (!(newTransform.getClass == oldTransform.getClass)) {
            resultNX = new NXTransformations(resultNX, newTransform, i, nxCopied)
            nxCopied = true
          }
        }
        i = mutationPlan(i + 1)
      }
      if (allDone) null
      else if (!transformersCopied) info
      else new TransformerInfo(result, resultNX, info.group)
    }

    val prepForIdent: Mutator[Ident] = (trans, tree, ctx) => trans.prepareForIdent(tree)(ctx)
    val prepForSelect: Mutator[Select] = (trans, tree, ctx) => trans.prepareForSelect(tree)(ctx)
    val prepForThis: Mutator[This] = (trans, tree, ctx) => trans.prepareForThis(tree)(ctx)
    val prepForSuper: Mutator[Super] = (trans, tree, ctx) => trans.prepareForSuper(tree)(ctx)
    val prepForApply: Mutator[Apply] = (trans, tree, ctx) => trans.prepareForApply(tree)(ctx)
    val prepForTypeApply: Mutator[TypeApply] = (trans, tree, ctx) => trans.prepareForTypeApply(tree)(ctx)
    val prepForNew: Mutator[New] = (trans, tree, ctx) => trans.prepareForNew(tree)(ctx)
    val prepForTyped: Mutator[Typed] = (trans, tree, ctx) => trans.prepareForTyped(tree)(ctx)
    val prepForAssign: Mutator[Assign] = (trans, tree, ctx) => trans.prepareForAssign(tree)(ctx)
    val prepForLiteral: Mutator[Literal] = (trans, tree, ctx) => trans.prepareForLiteral(tree)(ctx)
    val prepForBlock: Mutator[Block] = (trans, tree, ctx) => trans.prepareForBlock(tree)(ctx)
    val prepForIf: Mutator[If] = (trans, tree, ctx) => trans.prepareForIf(tree)(ctx)
    val prepForClosure: Mutator[Closure] = (trans, tree, ctx) => trans.prepareForClosure(tree)(ctx)
    val prepForMatch: Mutator[Match] = (trans, tree, ctx) => trans.prepareForMatch(tree)(ctx)
    val prepForCaseDef: Mutator[CaseDef] = (trans, tree, ctx) => trans.prepareForCaseDef(tree)(ctx)
    val prepForReturn: Mutator[Return] = (trans, tree, ctx) => trans.prepareForReturn(tree)(ctx)
    val prepForTry: Mutator[Try] = (trans, tree, ctx) => trans.prepareForTry(tree)(ctx)
    val prepForSeqLiteral: Mutator[SeqLiteral] = (trans, tree, ctx) => trans.prepareForSeqLiteral(tree)(ctx)
    val prepForInlined: Mutator[Inlined] = (trans, tree, ctx) => trans.prepareForInlined(tree)(ctx)
    val prepForTypeTree: Mutator[TypeTree] = (trans, tree, ctx) => trans.prepareForTypeTree(tree)(ctx)
    val prepForBind: Mutator[Bind] = (trans, tree, ctx) => trans.prepareForBind(tree)(ctx)
    val prepForAlternative: Mutator[Alternative] = (trans, tree, ctx) => trans.prepareForAlternative(tree)(ctx)
    val prepForUnApply: Mutator[UnApply] = (trans, tree, ctx) => trans.prepareForUnApply(tree)(ctx)
    val prepForValDef: Mutator[ValDef] = (trans, tree, ctx) => trans.prepareForValDef(tree)(ctx)
    val prepForDefDef: Mutator[DefDef] = (trans, tree, ctx) => trans.prepareForDefDef(tree)(ctx)
    val prepForTypeDef: Mutator[TypeDef] = (trans, tree, ctx) => trans.prepareForTypeDef(tree)(ctx)
    val prepForTemplate: Mutator[Template] = (trans, tree, ctx) => trans.prepareForTemplate(tree)(ctx)
    val prepForPackageDef: Mutator[PackageDef] = (trans, tree, ctx) => trans.prepareForPackageDef(tree)(ctx)
    val prepForStats: Mutator[List[Tree]] = (trans, trees, ctx) => trans.prepareForStats(trees)(ctx)
    val prepForUnit: Mutator[Tree] = (trans, tree, ctx) => trans.prepareForUnit(tree)(ctx)

    val initialTransformationsCache = miniPhases.zipWithIndex.map {
      case (miniPhase, id) =>
        miniPhase.idx = id
        miniPhase.treeTransform
    }

    val initialInfoCache = new TransformerInfo(initialTransformationsCache, new NXTransformations(initialTransformationsCache), this)

    def macroTransform(t: Tree)(implicit ctx: Context): Tree = {
      val info = initialInfoCache
      implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForUnit, info.nx.nxPrepUnit, t, 0)
      if (mutatedInfo eq null) t
      else goUnit(transform(t, mutatedInfo, 0), mutatedInfo.nx.nxTransUnit(0))
    }

    @tailrec
    final private[TreeTransforms] def goIdent(tree: Ident, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformIdent(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Ident => goIdent(t, info.nx.nxTransIdent(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goSelect(tree: Select, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformSelect(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Select => goSelect(t, info.nx.nxTransSelect(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goThis(tree: This, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformThis(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: This => goThis(t, info.nx.nxTransThis(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goSuper(tree: Super, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformSuper(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Super => goSuper(t, info.nx.nxTransSuper(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goApply(tree: Apply, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformApply(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Apply => goApply(t, info.nx.nxTransApply(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goTypeApply(tree: TypeApply, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformTypeApply(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: TypeApply => goTypeApply(t, info.nx.nxTransTypeApply(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goNew(tree: New, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformNew(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: New => goNew(t, info.nx.nxTransNew(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goTyped(tree: Typed, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformTyped(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Typed => goTyped(t, info.nx.nxTransTyped(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goAssign(tree: Assign, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformAssign(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Assign => goAssign(t, info.nx.nxTransAssign(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goLiteral(tree: Literal, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformLiteral(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Literal => goLiteral(t, info.nx.nxTransLiteral(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goBlock(tree: Block, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformBlock(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Block => goBlock(t, info.nx.nxTransBlock(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goIf(tree: If, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformIf(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: If => goIf(t, info.nx.nxTransIf(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goClosure(tree: Closure, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformClosure(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Closure => goClosure(t, info.nx.nxTransClosure(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goMatch(tree: Match, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformMatch(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Match => goMatch(t, info.nx.nxTransMatch(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goCaseDef(tree: CaseDef, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformCaseDef(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: CaseDef => goCaseDef(t, info.nx.nxTransCaseDef(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goReturn(tree: Return, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformReturn(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Return => goReturn(t, info.nx.nxTransReturn(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goTry(tree: Try, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformTry(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Try => goTry(t, info.nx.nxTransTry(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goSeqLiteral(tree: SeqLiteral, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformSeqLiteral(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: SeqLiteral => goSeqLiteral(t, info.nx.nxTransSeqLiteral(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goInlined(tree: Inlined, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformInlined(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Inlined => goInlined(t, info.nx.nxTransInlined(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goTypeTree(tree: TypeTree, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformTypeTree(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: TypeTree => goTypeTree(t, info.nx.nxTransTypeTree(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goBind(tree: Bind, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformBind(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Bind => goBind(t, info.nx.nxTransBind(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goAlternative(tree: Alternative, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformAlternative(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Alternative => goAlternative(t, info.nx.nxTransAlternative(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goValDef(tree: ValDef, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformValDef(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: ValDef => goValDef(t, info.nx.nxTransValDef(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goDefDef(tree: DefDef, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformDefDef(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: DefDef => goDefDef(t, info.nx.nxTransDefDef(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goUnApply(tree: UnApply, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformUnApply(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: UnApply => goUnApply(t, info.nx.nxTransUnApply(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goTypeDef(tree: TypeDef, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformTypeDef(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: TypeDef => goTypeDef(t, info.nx.nxTransTypeDef(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goTemplate(tree: Template, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformTemplate(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: Template => goTemplate(t, info.nx.nxTransTemplate(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goPackageDef(tree: PackageDef, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        trans.transformPackageDef(tree)(ctx.withPhase(trans.treeTransformPhase), info) match {
          case t: PackageDef => goPackageDef(t, info.nx.nxTransPackageDef(cur + 1))
          case t => transformSingle(t, cur + 1)
        }
      } else tree
    }

    @tailrec
    final private[TreeTransforms] def goUnit(tree: Tree, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        val t = trans.transformUnit(tree)(ctx.withPhase(trans.treeTransformPhase), info)
        goUnit(t, info.nx.nxTransUnit(cur + 1))
      } else tree
    }

    final private[TreeTransforms] def goOther(tree: Tree, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        val t = trans.transformOther(tree)(ctx.withPhase(trans.treeTransformPhase), info)
        transformSingle(t, cur + 1)
      } else tree
    }

    final private[TreeTransforms] def goNamed(tree: NameTree, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree =
      tree match {
        case tree: Ident => goIdent(tree, info.nx.nxTransIdent(cur))
        case tree: Select => goSelect(tree, info.nx.nxTransSelect(cur))
        case tree: Bind => goBind(tree, cur)
        case tree: ValDef if !tree.isEmpty => goValDef(tree, info.nx.nxTransValDef(cur))
        case tree: DefDef => goDefDef(tree, info.nx.nxTransDefDef(cur))
        case tree: TypeDef => goTypeDef(tree, info.nx.nxTransTypeDef(cur))
        case _ => tree
      }

    final private[TreeTransforms] def goUnnamed(tree: Tree, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree =
      tree match {
        case tree: This => goThis(tree, info.nx.nxTransThis(cur))
        case tree: Super => goSuper(tree, info.nx.nxTransSuper(cur))
        case tree: Apply => goApply(tree, info.nx.nxTransApply(cur))
        case tree: TypeApply => goTypeApply(tree, info.nx.nxTransTypeApply(cur))
        case tree: Literal => goLiteral(tree, info.nx.nxTransLiteral(cur))
        case tree: New => goNew(tree, info.nx.nxTransNew(cur))
        case tree: Typed => goTyped(tree, info.nx.nxTransTyped(cur))
        case tree: Assign => goAssign(tree, info.nx.nxTransAssign(cur))
        case tree: Block => goBlock(tree, info.nx.nxTransBlock(cur))
        case tree: If => goIf(tree, info.nx.nxTransIf(cur))
        case tree: Closure => goClosure(tree, info.nx.nxTransClosure(cur))
        case tree: Match => goMatch(tree, info.nx.nxTransMatch(cur))
        case tree: CaseDef => goCaseDef(tree, info.nx.nxTransCaseDef(cur))
        case tree: Return => goReturn(tree, info.nx.nxTransReturn(cur))
        case tree: Try => goTry(tree, info.nx.nxTransTry(cur))
        case tree: SeqLiteral => goSeqLiteral(tree, info.nx.nxTransSeqLiteral(cur))
        case tree: Inlined => goInlined(tree, info.nx.nxTransInlined(cur))
        case tree: TypeTree => goTypeTree(tree, info.nx.nxTransTypeTree(cur))
        case tree: Alternative => goAlternative(tree, info.nx.nxTransAlternative(cur))
        case tree: UnApply => goUnApply(tree, info.nx.nxTransUnApply(cur))
        case tree: Template => goTemplate(tree, info.nx.nxTransTemplate(cur))
        case tree: PackageDef => goPackageDef(tree, info.nx.nxTransPackageDef(cur))
        case Thicket(trees) => tree
        case tree => goOther(tree, info.nx.nxTransOther(cur))
      }

    final private[TreeTransforms] def transformSingle(tree: Tree, cur: Int)(implicit ctx: Context, info: TransformerInfo): Tree =
      if (cur < info.transformers.length) {
        tree match {
          // split one big match into 2 smaller ones
          case tree: NameTree => goNamed(tree, cur)
          case tree => goUnnamed(tree, cur)
        }
      } else tree

    // TODO merge with localCtx in MacroTransform
    // Generally: If we will keep MacroTransform, merge common behavior with TreeTransform
    def localContext(sym: Symbol)(implicit ctx: Context) = {
      val owner = if (sym is PackageVal) sym.moduleClass else sym
      ctx.fresh.setOwner(owner)
    }

    final private[TreeTransforms] def transformNamed(tree: NameTree, info: TransformerInfo, cur: Int)(implicit ctx: Context): Tree =
      tree match {
        case tree: Ident =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForIdent, info.nx.nxPrepIdent, tree, cur)
            // Dotty deviation: implicits need explicit type
          if (mutatedInfo eq null) tree
          else goIdent(tree, mutatedInfo.nx.nxTransIdent(cur))
        case tree: Select =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForSelect, info.nx.nxPrepSelect, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val qual = transform(tree.qualifier, mutatedInfo, cur)
            goSelect(cpy.Select(tree)(qual, tree.name), mutatedInfo.nx.nxTransSelect(cur))
          }
        case tree: ValDef if !tree.isEmpty => // As a result of discussing with Martin: emptyValDefs shouldn't be copied // NAME
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForValDef, info.nx.nxPrepValDef, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val nestedCtx = if (tree.symbol.exists) localContext(tree.symbol) else ctx
            val tpt = transform(tree.tpt, mutatedInfo, cur)(nestedCtx)
            val rhs = transform(tree.rhs, mutatedInfo, cur)(nestedCtx)
            goValDef(cpy.ValDef(tree)(tree.name, tpt, rhs), mutatedInfo.nx.nxTransValDef(cur))
          }
        case tree: DefDef =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForDefDef, info.nx.nxPrepDefDef, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val nestedCtx = localContext(tree.symbol)
            val tparams = transformSubTrees(tree.tparams, mutatedInfo, cur)(nestedCtx)
            val vparams = tree.vparamss.mapConserve(x => transformSubTrees(x, mutatedInfo, cur)(nestedCtx))
            val tpt = transform(tree.tpt, mutatedInfo, cur)(nestedCtx)
            val rhs = transform(tree.rhs, mutatedInfo, cur)(nestedCtx)
            goDefDef(cpy.DefDef(tree)(tree.name, tparams, vparams, tpt, rhs), mutatedInfo.nx.nxTransDefDef(cur))
          }
        case tree: TypeDef =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForTypeDef, info.nx.nxPrepTypeDef, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val rhs = transform(tree.rhs, mutatedInfo, cur)(localContext(tree.symbol))
            goTypeDef(cpy.TypeDef(tree)(tree.name, rhs), mutatedInfo.nx.nxTransTypeDef(cur))
          }
        case tree: Bind =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForBind, info.nx.nxPrepBind, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val body = transform(tree.body, mutatedInfo, cur)
            goBind(cpy.Bind(tree)(tree.name, body), mutatedInfo.nx.nxTransBind(cur))
          }
        case _ =>
          tree
      }

    final private[TreeTransforms] def transformUnnamed(tree: Tree, info: TransformerInfo, cur: Int)(implicit ctx: Context): Tree =
      tree match {
        case tree: Apply =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForApply, info.nx.nxPrepApply, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val fun = transform(tree.fun, mutatedInfo, cur)
            val args = transformSubTrees(tree.args, mutatedInfo, cur)
            goApply(cpy.Apply(tree)(fun, args), mutatedInfo.nx.nxTransApply(cur))
          }
        case tree: TypeTree =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForTypeTree, info.nx.nxPrepTypeTree, tree, cur)
          if (mutatedInfo eq null) tree
          else goTypeTree(tree, mutatedInfo.nx.nxTransTypeTree(cur))
        case Thicket(trees) =>
          cpy.Thicket(tree)(transformTrees(trees, info, cur))
        case tree: This =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForThis, info.nx.nxPrepThis, tree, cur)
          if (mutatedInfo eq null) tree
          else goThis(tree, mutatedInfo.nx.nxTransThis(cur))
        case tree: Literal =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForLiteral, info.nx.nxPrepLiteral, tree, cur)
          if (mutatedInfo eq null) tree
          else goLiteral(tree, mutatedInfo.nx.nxTransLiteral(cur))
        case tree: Block =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForBlock, info.nx.nxPrepBlock, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val stats = transformStats(tree.stats, ctx.owner, mutatedInfo, cur)
            val expr = transform(tree.expr, mutatedInfo, cur)
            goBlock(cpy.Block(tree)(stats, expr), mutatedInfo.nx.nxTransBlock(cur))
          }
        case tree: TypeApply =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForTypeApply, info.nx.nxPrepTypeApply, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val fun = transform(tree.fun, mutatedInfo, cur)
            val args = transformTrees(tree.args, mutatedInfo, cur)
            goTypeApply(cpy.TypeApply(tree)(fun, args), mutatedInfo.nx.nxTransTypeApply(cur))
          }
        case tree: If =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForIf, info.nx.nxPrepIf, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val cond = transform(tree.cond, mutatedInfo, cur)
            val thenp = transform(tree.thenp, mutatedInfo, cur)
            val elsep = transform(tree.elsep, mutatedInfo, cur)
            goIf(cpy.If(tree)(cond, thenp, elsep), mutatedInfo.nx.nxTransIf(cur))
          }
        case tree: New =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForNew, info.nx.nxPrepNew, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val tpt = transform(tree.tpt, mutatedInfo, cur)
            goNew(cpy.New(tree)(tpt), mutatedInfo.nx.nxTransNew(cur))
          }
        case tree: Typed =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForTyped, info.nx.nxPrepTyped, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val expr = transform(tree.expr, mutatedInfo, cur)
            val tpt = transform(tree.tpt, mutatedInfo, cur)
            goTyped(cpy.Typed(tree)(expr, tpt), mutatedInfo.nx.nxTransTyped(cur))
          }
        case tree: CaseDef =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForCaseDef, info.nx.nxPrepCaseDef, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val pat = transform(tree.pat, mutatedInfo, cur)(ctx.addMode(Mode.Pattern))
            val guard = transform(tree.guard, mutatedInfo, cur)
            val body = transform(tree.body, mutatedInfo, cur)
            goCaseDef(cpy.CaseDef(tree)(pat, guard, body), mutatedInfo.nx.nxTransCaseDef(cur))
          }
        case tree: Closure =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForClosure, info.nx.nxPrepClosure, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val env = transformTrees(tree.env, mutatedInfo, cur)
            val meth = transform(tree.meth, mutatedInfo, cur)
            val tpt = transform(tree.tpt, mutatedInfo, cur)
            goClosure(cpy.Closure(tree)(env, meth, tpt), mutatedInfo.nx.nxTransClosure(cur))
          }
        case tree: Assign =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForAssign, info.nx.nxPrepAssign, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val lhs = transform(tree.lhs, mutatedInfo, cur)
            val rhs = transform(tree.rhs, mutatedInfo, cur)
            goAssign(cpy.Assign(tree)(lhs, rhs), mutatedInfo.nx.nxTransAssign(cur))
          }
        case tree: SeqLiteral =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForSeqLiteral, info.nx.nxPrepSeqLiteral, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val elems = transformTrees(tree.elems, mutatedInfo, cur)
            val elemtpt = transform(tree.elemtpt, mutatedInfo, cur)
            goSeqLiteral(cpy.SeqLiteral(tree)(elems, elemtpt), mutatedInfo.nx.nxTransSeqLiteral(cur))
          }
        case tree: Super =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForSuper, info.nx.nxPrepSuper, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val qual = transform(tree.qual, mutatedInfo, cur)
            goSuper(cpy.Super(tree)(qual, tree.mix), mutatedInfo.nx.nxTransSuper(cur))
          }
        case tree: Template =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForTemplate, info.nx.nxPrepTemplate, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val constr = transformSub(tree.constr, mutatedInfo, cur)
            val parents = transformTrees(tree.parents, mutatedInfo, cur)(ctx.superCallContext)
            val self = transformSub(tree.self, mutatedInfo, cur)
            val body = transformStats(tree.body, tree.symbol, mutatedInfo, cur)
            goTemplate(cpy.Template(tree)(constr, parents, self, body), mutatedInfo.nx.nxTransTemplate(cur))
          }
        case tree: Match =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForMatch, info.nx.nxPrepMatch, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val selector = transform(tree.selector, mutatedInfo, cur)
            val cases = transformSubTrees(tree.cases, mutatedInfo, cur)
            goMatch(cpy.Match(tree)(selector, cases), mutatedInfo.nx.nxTransMatch(cur))
          }
        case tree: UnApply =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForUnApply, info.nx.nxPrepUnApply, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val fun = transform(tree.fun, mutatedInfo, cur)
            val implicits = transformTrees(tree.implicits, mutatedInfo, cur)
            val patterns = transformTrees(tree.patterns, mutatedInfo, cur)
            goUnApply(cpy.UnApply(tree)(fun, implicits, patterns), mutatedInfo.nx.nxTransUnApply(cur))
          }
        case tree: PackageDef =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForPackageDef, info.nx.nxPrepPackageDef, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val nestedCtx = localContext(tree.symbol)
            val pid = transformSub(tree.pid, mutatedInfo, cur)
            val stats = transformStats(tree.stats, tree.symbol, mutatedInfo, cur)(nestedCtx)
            goPackageDef(cpy.PackageDef(tree)(pid, stats), mutatedInfo.nx.nxTransPackageDef(cur))
          }
        case tree: Try =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForTry, info.nx.nxPrepTry, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val block = transform(tree.expr, mutatedInfo, cur)
            val cases1 = tree.cases.mapConserve(transform(_, mutatedInfo, cur)).asInstanceOf[List[CaseDef]]
            val finalizer = transform(tree.finalizer, mutatedInfo, cur)
            goTry(cpy.Try(tree)(block, cases1, finalizer), mutatedInfo.nx.nxTransTry(cur))
          }
        case tree: Inlined =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForInlined, info.nx.nxPrepInlined, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val bindings = transformSubTrees(tree.bindings, mutatedInfo, cur)
            val expansion = transform(tree.expansion, mutatedInfo, cur)(inlineContext(tree))
            goInlined(cpy.Inlined(tree)(tree.call, bindings, expansion), mutatedInfo.nx.nxTransInlined(cur))
          }
        case tree: Return =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForReturn, info.nx.nxPrepReturn, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val expr = transform(tree.expr, mutatedInfo, cur)
            val from = tree.from
              // don't transform the `from` part, as this is not a normal ident, but
              // a pointer to the enclosing method. Transforming this as a normal ident
              // can go wrong easily. If a transformation is needed, it should be
              // the responsibility of the transformReturn method to handle this also.
            goReturn(cpy.Return(tree)(expr, from), mutatedInfo.nx.nxTransReturn(cur))
          }
        case tree: Alternative =>
          implicit val mutatedInfo: TransformerInfo = mutateTransformers(info, prepForAlternative, info.nx.nxPrepAlternative, tree, cur)
          if (mutatedInfo eq null) tree
          else {
            val trees = transformTrees(tree.trees, mutatedInfo, cur)
            goAlternative(cpy.Alternative(tree)(trees), mutatedInfo.nx.nxTransAlternative(cur))
          }
        case tree =>
          implicit val originalInfo: TransformerInfo = info
          goOther(tree, info.nx.nxTransOther(cur))
      }

    private var crashingTree: Tree = EmptyTree

    def transform(tree: Tree, info: TransformerInfo, cur: Int)(implicit ctx: Context): Tree = ctx.traceIndented(s"transforming ${tree.show} at ${ctx.phase}", transforms, show = true) {
      try
        if (cur < info.transformers.length) {
          util.Stats.record("TreeTransform.transform")
          // if cur > 0 then some of the symbols can be created by already performed transformations
          // this means that their denotations could not exists in previous period
          val pctx = ctx.withPhase(info.transformers(cur).treeTransformPhase)
          tree match {
            //split one big match into 2 smaller ones
            case tree: NameTree => transformNamed(tree, info, cur)(pctx)
            case tree => transformUnnamed(tree, info, cur)(pctx)
          }
        } else tree
      catch {
        case NonFatal(ex) =>
          if (tree ne crashingTree) {
            crashingTree = tree
            println(i"exception while transforming $tree of class ${tree.getClass} # ${tree.uniqueId}")
          }
          throw ex
      }
    }

    @tailrec
    final private[TreeTransforms] def goStats(trees: List[Tree], cur: Int)(implicit ctx: Context, info: TransformerInfo): List[Tree] = {
      if (cur < info.transformers.length) {
        val trans = info.transformers(cur)
        val stats = trans.transformStats(trees)(ctx.withPhase(trans.treeTransformPhase), info)
        goStats(stats, info.nx.nxTransStats(cur + 1))
      } else trees
    }

    def transformStats(trees: List[Tree], exprOwner: Symbol, info: TransformerInfo, current: Int)(implicit ctx: Context): List[Tree] = {
      val newInfo = mutateTransformers(info, prepForStats, info.nx.nxPrepStats, trees, current)
      def transformStat(stat: Tree): Tree = stat match {
        case _: Import | _: DefTree => transform(stat, newInfo, current)
        case Thicket(stats) => cpy.Thicket(stat)(stats mapConserve transformStat)
        case _ => transform(stat, newInfo, current)(ctx.exprContext(stat, exprOwner))
      }
      val newTrees = flatten(trees.mapconserve(transformStat))
      goStats(newTrees, newInfo.nx.nxTransStats(current))(ctx, newInfo)
    }

    def transformTrees(trees: List[Tree], info: TransformerInfo, current: Int)(implicit ctx: Context): List[Tree] =
      flatten(trees mapConserve (x => transform(x, info, current)))

    def transformSub[Tr <: Tree](tree: Tr, info: TransformerInfo, current: Int)(implicit ctx: Context): Tr =
      transform(tree, info, current).asInstanceOf[Tr]

    def transformSubTrees[Tr <: Tree](trees: List[Tr], info: TransformerInfo, current: Int)(implicit ctx: Context): List[Tr] =
      transformTrees(trees, info, current)(ctx).asInstanceOf[List[Tr]]
  }
}
