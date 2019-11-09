package dotty.tools
package dotc
package typer

import core._
import Types._, Contexts._, Symbols._, Decorators._, Constants._
import annotation.tailrec
import util.Property

/** Operations for implementing a flow analysis for nullability */
object Nullables with
  import ast.tpd._

  /** A set of paths that are known to be not null */
  type Excluded = Set[TermRef]

  /** A pair of not-null sets, depending on whether a condition is `true` or `false` */
  case class EitherExcluded(ifTrue: Excluded, ifFalse: Excluded) with
    def isEmpty = ifTrue.isEmpty && ifFalse.isEmpty

  val NoneExcluded = EitherExcluded(Set(), Set())

  /** An attachment that represents conditional flow facts established
   *  by this tree, which represents a condition.
   */
  private[typer] val CondExcluded = Property.StickyKey[Nullables.EitherExcluded]

   /** An attachment that represents unconditional flow facts established
    *  by this tree.
    */
  private[typer] val AlwaysExcluded = Property.StickyKey[Nullables.Excluded]

  /** An extractor for null comparisons */
  object CompareNull with

    /** Matches one of
     *
     *    tree == null, tree eq null, null == tree, null eq tree
     *    tree != null, tree ne null, null != tree, null ne tree
     *
     *  The second boolean result is true for equality tests, false for inequality tests
     */
    def unapply(tree: Tree)(given Context): Option[(Tree, Boolean)] = tree match
      case Apply(Select(l, _), Literal(Constant(null)) :: Nil) =>
        testSym(tree.symbol, l)
      case Apply(Select(Literal(Constant(null)), _), r :: Nil) =>
        testSym(tree.symbol, r)
      case _ =>
        None

    private def testSym(sym: Symbol, operand: Tree)(given Context) =
      if sym == defn.Any_== || sym == defn.Object_eq then Some((operand, true))
      else if sym == defn.Any_!= || sym == defn.Object_ne then Some((operand, false))
      else None

  end CompareNull

  /** An extractor for null-trackable references */
  object TrackedRef
    def unapply(tree: Tree)(given Context): Option[TermRef] = tree.typeOpt match
      case ref: TermRef if isTracked(ref) => Some(ref)
      case _ => None
  end TrackedRef

  /** Is given reference tracked for nullability? */
  def isTracked(ref: TermRef)(given Context) = ref.isStable

  def afterPatternContext(sel: Tree, pat: Tree)(given ctx: Context) = (sel, pat) match
    case (TrackedRef(ref), Literal(Constant(null))) => ctx.addExcluded(Set(ref))
    case _ => ctx

  def caseContext(sel: Tree, pat: Tree)(given ctx: Context): Context = sel match
    case TrackedRef(ref) if matchesNotNull(pat) => ctx.addExcluded(Set(ref))
    case _ => ctx

  private def matchesNotNull(pat: Tree)(given Context): Boolean = pat match
    case _: Typed | _: UnApply => true
    case Alternative(pats) => pats.forall(matchesNotNull)
    // TODO: Add constant pattern if the constant type is not nullable
    case _ => false

  given (excluded: List[Excluded])
    def containsRef(ref: TermRef): Boolean =
      excluded.exists(_.contains(ref))

    def containsAll(refs: Set[TermRef]): Boolean =
      refs.forall(excluded.containsRef(_))

  given (tree: Tree)

    /* The `tree` with added attachment stating that all paths in `refs` are not-null */
    def withNotNullRefs(refs: Excluded): tree.type =
      if refs.nonEmpty then tree.putAttachment(AlwaysExcluded, refs)
      tree

    /* The paths that are known to be not null after execution of `tree` terminates normally */
    def notNullRefs(given Context): Excluded =
      stripInlined(tree).getAttachment(AlwaysExcluded) match
        case Some(excl) if !curCtx.erasedTypes => excl
        case _ => Set.empty

    /** The paths that are known to be not null if the condition represented
     *  by `tree` yields `true` or `false`. Two empty sets if `tree` is not
     *  a condition.
     */
    def condNotNullRefs(given Context): EitherExcluded =
      stripBlock(tree).getAttachment(CondExcluded) match
        case Some(excl) if !curCtx.erasedTypes => excl
        case _ => NoneExcluded

    /** The current context augmented with nullability information of `tree` */
    def nullableContext(given Context): Context =
      val excl = tree.notNullRefs
      if excl.isEmpty then curCtx else curCtx.addExcluded(excl)

    /** The current context augmented with nullability information,
     *  assuming the result of the condition represented by `tree` is the same as
     *  the value of `tru`. The current context if `tree` is not a condition.
     */
    def nullableContext(tru: Boolean)(given Context): Context =
      val excl = tree.condNotNullRefs
      if excl.isEmpty then curCtx
      else curCtx.addExcluded(if tru then excl.ifTrue else excl.ifFalse)

    /** The context to use for the arguments of the function represented by `tree`.
     *  This is the current context, augmented with nullability information
     *  of the left argument, if the application is a boolean `&&` or `||`.
     */
    def nullableInArgContext(given Context): Context = tree match
      case Select(x, _) if !curCtx.erasedTypes =>
        if tree.symbol == defn.Boolean_&& then x.nullableContext(true)
        else if tree.symbol == defn.Boolean_|| then x.nullableContext(false)
        else curCtx
      case _ => curCtx

    /** The `tree` augmented with nullability information in an attachment.
     *  The following operations lead to nullability info being recorded:
     *
     *  1. Null tests using `==`, `!=`, `eq`, `ne`, if the compared entity is
     *     a path (i.e. a stable TermRef)
     *  2. Boolean &&, ||, !
     */
    def computeNullable()(given Context): tree.type =
      def setExcluded(ifTrue: Excluded, ifFalse: Excluded) =
        tree.putAttachment(CondExcluded, EitherExcluded(ifTrue, ifFalse))
      if !curCtx.erasedTypes then tree match
        case CompareNull(TrackedRef(ref), testEqual) =>
          if testEqual then setExcluded(Set(), Set(ref))
          else setExcluded(Set(ref), Set())
        case Apply(Select(x, _), y :: Nil) =>
          val xc = x.condNotNullRefs
          val yc = y.condNotNullRefs
          if !(xc.isEmpty && yc.isEmpty) then
            if tree.symbol == defn.Boolean_&& then
              setExcluded(xc.ifTrue | yc.ifTrue, xc.ifFalse & yc.ifFalse)
            else if tree.symbol == defn.Boolean_|| then
              setExcluded(xc.ifTrue & yc.ifTrue, xc.ifFalse | yc.ifFalse)
        case Select(x, _) if tree.symbol == defn.Boolean_! =>
          val xc = x.condNotNullRefs
          if !xc.isEmpty then
            setExcluded(xc.ifFalse, xc.ifTrue)
        case _ =>
      tree

    /** Compute nullability information for this tree and all its subtrees */
    def computeNullableDeeply()(given Context): Unit =
      new TreeTraverser {
        def traverse(tree: Tree)(implicit ctx: Context) =
          traverseChildren(tree)
          tree.computeNullable()
      }.traverse(tree)

end Nullables
