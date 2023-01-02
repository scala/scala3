package dotty.tools
package dotc
package transform

import ast.{Trees, tpd}
import core.*
import Decorators.*
import NameKinds.BoundaryName
import MegaPhase._
import Types._, Contexts._, Flags._, DenotTransformers._
import Symbols._, StdNames._, Trees._
import util.Property
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
    /** The number of other references to assocated label */
    var otherRefs: Int = 0

  private val LabelUsages = new Property.Key[Map[Symbol, LabelUsage]]
  private val LabelsShadowedByTry = new Property.Key[Set[Symbol]]

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

  import tpd._

  override def phaseName: String = DropBreaks.name

  override def description: String = DropBreaks.description

  override def runsAfterGroupsOf: Set[String] = Set(ElimByName.name)
    // we want by-name parameters to be converted to closures

  private object LabelTry:

    object GuardedThrow:

      /** `(ex, local)` provided `expr` matches
       *
       *      if ex.label.eq(local) then ex.value else throw ex
       */
      def unapply(expr: Tree)(using Context): Option[(Symbol, Symbol)] = stripTyped(expr) match
        case If(
          Apply(Select(Select(ex: Ident, label), eq), (lbl @ Ident(local)) :: Nil),
          Select(ex2: Ident, value),
          Apply(throww, (ex3: Ident) :: Nil))
        if label == nme.label && eq == nme.eq && local == nme.local && value == nme.value
            && throww.symbol == defn.throwMethod
            && ex.symbol == ex2.symbol && ex.symbol == ex3.symbol =>
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
    def unapply(tree: Tree)(using Context): Option[(Symbol, Tree)] = tree match
      case Block((vd @ ValDef(nme.local, _, _)) :: Nil, LabelTry(caughtAndRhs))
      if vd.symbol.info.isRef(defn.LabelClass) && vd.symbol == caughtAndRhs._1 =>
        Some(caughtAndRhs)
      case _ =>
        None
  end BreakBoundary

  private object BreakThrow:

    /** `(local, arg)` provided `tree` matches inlined
     *
     *    val Label_this: ... = local
     *    throw new Break[...](Label_this, arg)
     */
    def unapply(tree: Tree)(using Context): Option[(Symbol, Tree)] = tree match
      case Inlined(_,
        (vd @ ValDef(label_this1, _, id: Ident)):: Nil,
        Apply(throww, Apply(constr, Inlined(_, _, Ident(label_this2)) :: arg :: Nil) :: Nil))
      if throww.symbol == defn.throwMethod
          && label_this1 == nme.Label_this && label_this2 == nme.Label_this
          && id.symbol.name == nme.local
          && constr.symbol.isClassConstructor && constr.symbol.owner == defn.BreakClass =>
        Some((id.symbol, arg))
      case _ =>
        None
  end BreakThrow

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
      val mapSoFar = ctx.property(LabelUsages).getOrElse(Map.empty)
      val goto = newSymbol(ctx.owner, BoundaryName.fresh(), Synthetic | Label, tree.tpe)
      ctx.fresh.setProperty(LabelUsages,
        mapSoFar.updated(label, LabelUsage(goto, ctx.owner.enclosingMethod)))
    case _ =>
      ctx

  /** If `tree` is not a LabeledTry, include all enclosing labels in the
   *  `LabelsShadowedByTry` context property. This means that breaks to these
   *  labels will not be translated to labeled returns in the body of the try.
   */
  override def prepareForTry(tree: Try)(using Context): Context = tree match
    case LabelTry(_, _) => ctx
    case _ => ctx.property(LabelUsages) match
      case Some(usesMap) =>
        val setSoFar = ctx.property(LabelsShadowedByTry).getOrElse(Set.empty)
        ctx.fresh.setProperty(LabelsShadowedByTry, setSoFar ++ usesMap.keysIterator)
      case _ => ctx

  /** If `tree` is a BreakBoundary, transform it as follows:
   *   - Wrap it in a labeled block if its label has local uses
   *   - Drop the try/catch if its label has no other uses
   */
  override def transformBlock(tree: Block)(using Context): Tree = tree match
    case BreakBoundary(label, expr) =>
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

  /** Rewrite a BreakThrow
   *
   *     val Label_this: ... = local
   *     throw new Break[...](Label_this, arg)
   *
   *  where `local` is defined in the current method and is not included in
   *  LabeldShowedByTry to
   *
   *     return[target] arg
   *
   *  where `target` is the `goto` return label associated with `local`.
   *  Adjust associated ref counts accordingly. The local refcount is increased
   *  and the non-local refcount is decreased, since `local` the `Label_this`
   *  binding containing `local` is dropped.
   */
  override def transformInlined(tree: Inlined)(using Context): Tree = tree match
    case BreakThrow(lbl, arg) =>
      report.log(i"trans inlined $arg, ${arg.source}, ${ctx.outer.source}, ${tree.source}")
      labelUsage(lbl) match
        case Some(uses: LabelUsage)
        if uses.enclMeth == ctx.owner.enclosingMethod
            && !ctx.property(LabelsShadowedByTry).getOrElse(Set.empty).contains(lbl)
          =>
          uses.otherRefs -= 1
          uses.returnRefs += 1
          cpy.Inlined(tree)(tree.call, Nil,
            inContext(ctx.withSource(tree.expansion.source)) {
              Return(arg, ref(uses.goto)).withSpan(arg.span)
            })
        case _ =>
          tree
    case _ =>
      tree

  /** If `tree` refers to an enclosing label, increase its non local recount.
   *  This increase is corrected in `transformInlined` if the reference turns
   *  out to be part of a BreakThrow to a local, non-shadowed label.
   */
  override def transformIdent(tree: Ident)(using Context): Tree =
    if tree.symbol.name == nme.local then
      for uses <- labelUsage(tree.symbol) do
        uses.otherRefs += 1
    tree

end DropBreaks
