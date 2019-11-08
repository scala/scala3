package dotty.tools
package dotc
package typer

import core._
import Types._, Contexts._, Symbols._, Decorators._
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
      tree.getAttachment(AlwaysExcluded) match
        case Some(excl) if !curCtx.isAfterTyper => excl
        case _ => Set.empty

    /** The paths that are known to be not null if the condition represented
     *  by `tree` yields `true` or `false`. Two empty sets if `tree` is not
     *  a condition.
     */
    def condNotNullRefs(given Context): EitherExcluded =
      stripBlock(tree).getAttachment(CondExcluded) match
        case Some(excl) if !curCtx.isAfterTyper => excl
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
      case Select(x, _) if !curCtx.isAfterTyper =>
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
    def computeNullable(given Context): tree.type =
      def setExcluded(ifTrue: Excluded, ifFalse: Excluded) =
        tree.putAttachment(CondExcluded, EitherExcluded(ifTrue, ifFalse))
      if !curCtx.isAfterTyper then tree match
        case ComparePathNull(ref, testEqual) =>
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
end Nullables
