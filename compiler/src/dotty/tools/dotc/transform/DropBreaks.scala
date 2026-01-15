package dotty.tools
package dotc
package transform

import ast.{Trees, tpd}
import core.*
import Decorators.*
import NameKinds.BoundaryName
import MegaPhase.*
import Types.*, Contexts.*, Flags.*, DenotTransformers.*
import Symbols.*, StdNames.*, Trees.*
import util.Property
import Constants.Constant
import Flags.MethodOrLazy

object DropBreaks:
  val name: String = "dropBreaks"
  val description: String = "replace local Break throws by labeled returns"

  /** Usage data and other info associated with a Label symbol.
   *  @param  goto     the return-label to use for a labeled return.
   *  @param  enclMeth the enclosing method
   */
  class LabelUsage(val goto: TermSymbol, val enclMeth: Symbol):
    /** The number of references to associated label that come from labeled returns */
    var returnRefs: Int = 0
    /** The number of other references to associated label */
    var otherRefs: Int = 0

    override def toString() = s"LabelUsage($goto, $enclMeth, returnRefs = $returnRefs, otherRefs = $otherRefs)"

  private val LabelUsages = new Property.Key[Map[Symbol, LabelUsage]]
  private val ShadowedLabels = new Property.Key[Set[Symbol]]

/** Rewrites local Break throws to labeled returns.
 *  Drops `try` statements on breaks if no other uses of its label remain.
 *  A Break throw with a `Label` created by some enclosing boundary is replaced
 *  with a labeled return if
 *
 *    - the throw and the boundary are in the same method, and
 *    - there is no try expression inside the boundary that encloses the throw.
 */
class DropBreaks extends MiniPhase:
  import DropBreaks.*

  import tpd.*

  override def phaseName: String = DropBreaks.name

  override def description: String = DropBreaks.description

  override def runsAfterGroupsOf: Set[String] = Set(ElimByName.name)
    // we want by-name parameters to be converted to closures

  /** The number of boundary nodes enclosing the currently analized tree. */
  private var enclosingBoundaries: Int = 0

  private object LabelTry:

    object GuardedThrow:

      /** `(ex, local)` provided `expr` matches
       *
       *      if ex.label.eq(local) then ex.value else throw ex
       */
      def unapply(expr: Tree)(using Context): Option[(Symbol, Symbol)] = stripTyped(expr) match
        case If(
          Apply(Select(ex: Ident, isSameLabelAs), (lbl @ Ident(local)) :: Nil),
          Select(ex2: Ident, value),
          Apply(throww, (ex3: Ident) :: Nil))
        if isSameLabelAs == nme.isSameLabelAs && local == nme.local && value == nme.value
            && throww.symbol == defn.throwMethod
            && ex.symbol == ex2.symbol && ex.symbol == ex3.symbol =>
          Some((ex.symbol, lbl.symbol))
        case If(
          Apply(Select(ex: Ident, isSameLabelAs), (lbl @ Ident(local)) :: Nil),
          Literal(_), // in the case where the value is constant folded
          Apply(throww, (ex3: Ident) :: Nil))
        if isSameLabelAs == nme.isSameLabelAs && local == nme.local
            && throww.symbol == defn.throwMethod
            && ex.symbol == ex3.symbol
            && expr.tpe.isSingleton =>
          Some((ex.symbol, lbl.symbol))
        case _ =>
          None
    end GuardedThrow

    /** `(local, body)` provided `tree` matches
     *
     *      try body
     *      catch case ex: Break =>
     *        if ex.label.eq(local) then ex.value else throw ex
     */
    def unapply(tree: Tree)(using Context): Option[(Symbol, Tree)] = stripTyped(tree) match
      case Try(body, CaseDef(pat @ Bind(_, Typed(_, tpt)), EmptyTree, GuardedThrow(exc, local)) :: Nil, EmptyTree)
      if tpt.tpe.isRef(defn.BreakClass) && exc == pat.symbol =>
        Some((local, body))
      case _ =>
        None
  end LabelTry

  private object BreakBoundary:

    /** `(local, body)` provided `tree` matches
     *
     *      { val local: Label[...] = ...; <LabelTry(local, body)> }
     */
    def unapply(tree: Tree)(using Context): Option[(Symbol, Tree)] = stripTyped(tree) match
      case Block((vd @ ValDef(nme.local, _, _)) :: Nil, LabelTry(caughtAndRhs))
      if vd.symbol.info.isRef(defn.LabelClass) && vd.symbol == caughtAndRhs._1 =>
        Some(caughtAndRhs)
      case _ =>
        None
  end BreakBoundary

  private object Break:

    private def isBreak(sym: Symbol)(using Context): Boolean =
      sym.name == nme.break && sym.owner == defn.boundaryModule.moduleClass

    /** `(local, arg)` provided `tree` matches
     *
     *     break[...](arg)(local)
     *
     *  or `(local, ())` provided `tree` matches
     *
     *     break()(local)
     */
    def unapply(tree: Tree)(using Context): Option[(Symbol, Tree)] = tree match
      case Apply(Apply(fn, args), id :: Nil)
      if isBreak(fn.symbol) =>
        stripInlined(id) match
          case id: Ident =>
            val arg = (args: @unchecked) match
              case arg :: Nil => arg
              case Nil => unitLiteral.withSpan(tree.span)
            Some((id.symbol, arg))
          case _ => None
      case _ => None
  end Break

  /** The LabelUsage data associated with `lbl` in the current context */
  private def labelUsage(lbl: Symbol)(using Context): Option[LabelUsage] =
    for
      usesMap <- ctx.property(LabelUsages)
      uses <- usesMap.get(lbl)
    yield
      uses

  /** If `tree` is a BreakBoundary, associate a fresh `LabelUsage` with its label. */
  override def prepareForBlock(tree: Block)(using Context): Context = tree match
    case BreakBoundary(label, _) =>
      enclosingBoundaries += 1
      val mapSoFar = ctx.property(LabelUsages).getOrElse(Map.empty)
      val goto = newSymbol(ctx.owner, BoundaryName.fresh(), Synthetic | Label, tree.tpe)
      ctx.fresh.setProperty(LabelUsages,
        mapSoFar.updated(label, LabelUsage(goto, ctx.owner.enclosingMethod)))
    case _ =>
      ctx

  /** Include all enclosing labels in the `ShadowedLabels` context property.
   *  This means that breaks to these labels will not be translated to labeled
   *  returns while this context is valid.
   */
  private def shadowLabels(using Context): Context =
    ctx.property(LabelUsages) match
      case Some(usesMap) =>
        val setSoFar = ctx.property(ShadowedLabels).getOrElse(Set.empty)
        ctx.fresh.setProperty(ShadowedLabels, setSoFar ++ usesMap.keysIterator)
      case _ => ctx

  /** Need to suppress labeled returns if there is an intervening try
   */
  override def prepareForTry(tree: Try)(using Context): Context =
    if enclosingBoundaries == 0 then ctx
    else tree match
      case LabelTry(_, _) => ctx
      case _ => shadowLabels

  override def prepareForValDef(tree: ValDef)(using Context): Context =
    if enclosingBoundaries != 0
        && tree.symbol.is(Lazy)
        && tree.symbol.owner == ctx.owner.enclosingMethod
    then shadowLabels // RHS be converted to a lambda
    else ctx

  /** If `tree` is a BreakBoundary, transform it as follows:
   *   - Wrap it in a labeled block if its label has local uses
   *   - Drop the try/catch if its label has no other uses
   */
  override def transformBlock(tree: Block)(using Context): Tree = tree match
    case BreakBoundary(label, expr) =>
      enclosingBoundaries -= 1
      val uses = ctx.property(LabelUsages).get(label)
      val tree1 =
        if uses.otherRefs > 1 then
          // one non-local ref is always in the catch clause; this one does not count
          tree
        else
          expr
      report.log(i"trans boundary block $label // ${uses.returnRefs}, ${uses.otherRefs}")
      if uses.returnRefs > 0 then Labeled(uses.goto, tree1) else tree1
    case _ =>
      tree

  private def isBreak(sym: Symbol)(using Context): Boolean =
    sym.name == nme.break && sym.owner == defn.boundaryModule.moduleClass

  private def transformBreak(tree: Tree, arg: Tree, lbl: Symbol)(using Context): Tree =
    report.log(i"transform break $tree/$arg/$lbl")
    labelUsage(lbl) match
      case Some(uses: LabelUsage)
      if uses.enclMeth == ctx.owner.enclosingMethod
          && !ctx.property(ShadowedLabels).getOrElse(Set.empty).contains(lbl)
        =>
        uses.otherRefs -= 1
        uses.returnRefs += 1
        Return(arg, ref(uses.goto)).withSpan(arg.span)
      case _ =>
        tree


  /** Rewrite a break call
   *
   *     break.apply[...](value)(using lbl)
   *
   *  where `lbl` is a label defined in the current method and is not included in
   *  ShadowedLabels to
   *
   *     return[target] arg
   *
   *  where `target` is the `goto` return label associated with `lbl`.
   *  Adjust associated ref counts accordingly. The local refcount is increased
   *  and the non-local refcount is decreased, since the `lbl` implicit argument
   *  to `break` is dropped.
   */
  override def transformApply(tree: Apply)(using Context): Tree =
    if enclosingBoundaries == 0 then tree
    else tree match
      case Break(lbl, arg) =>
        labelUsage(lbl) match
          case Some(uses: LabelUsage)
          if uses.enclMeth == ctx.owner.enclosingMethod
              && !ctx.property(ShadowedLabels).getOrElse(Set.empty).contains(lbl)
            =>
            uses.otherRefs -= 1
            uses.returnRefs += 1
            Return(arg, ref(uses.goto)).withSpan(arg.span)
          case _ => tree
      case _ => tree

  /** If `tree` refers to an enclosing label, increase its non local recount.
   *  This increase is corrected in `transformInlined` if the reference turns
   *  out to be part of a BreakThrow to a local, non-shadowed label.
   */
  override def transformIdent(tree: Ident)(using Context): Tree =
    if enclosingBoundaries != 0 then
      for uses <- labelUsage(tree.symbol) do
        uses.otherRefs += 1
    tree

end DropBreaks
