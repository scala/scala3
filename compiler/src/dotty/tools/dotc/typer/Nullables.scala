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

  type Excluded = Set[TermRef]

  case class EitherExcluded(ifTrue: Excluded, ifFalse: Excluded) with
    def isEmpty = ifTrue.isEmpty && ifFalse.isEmpty

  val NoneExcluded = EitherExcluded(Set(), Set())

  /** An attachment that represents conditional flow facts established
   *  by this tree, which represents a condition.
   */
  private[typer] val CondExcluded = Property.Key[Nullables.EitherExcluded]

   /** An attachment that represents unconditional flow facts established
    *  by this tree.
    */
  private[typer] val AlwaysExcluded = Property.Key[Nullables.Excluded]

  given (excluded: List[Excluded])
    def containsRef(ref: TermRef): Boolean =
      excluded.exists(_.contains(ref))

    def containsAll(refs: Set[TermRef]): Boolean =
      refs.forall(excluded.containsRef(_))

  given (tree: Tree)
    def withNotNullRefs(refs: Excluded): tree.type =
      if refs.nonEmpty then tree.putAttachment(AlwaysExcluded, refs)
      tree

    def notNullRefs(given Context): Excluded =
      tree.getAttachment(AlwaysExcluded) match
        case Some(cond) if !curCtx.isAfterTyper => cond
        case _ => Set.empty

    def condNotNullRefs(given Context): EitherExcluded =
      stripBlock(tree).getAttachment(CondExcluded) match
        case Some(cond) if !curCtx.isAfterTyper => cond
        case _ => NoneExcluded

    def nullableContext(given Context): Context =
      val excl = tree.notNullRefs
      if excl.isEmpty then curCtx else curCtx.addExcluded(excl)

    def nullableContext(tru: Boolean)(given Context): Context =
      val excl = tree.condNotNullRefs
      if excl.isEmpty then curCtx
      else curCtx.addExcluded(if tru then excl.ifTrue else excl.ifFalse)

    def nullableInArgContext(given Context): Context = tree match
      case Select(x, _) if !curCtx.isAfterTyper =>
        if tree.symbol == defn.Boolean_&& then x.nullableContext(true)
        else if tree.symbol == defn.Boolean_|| then x.nullableContext(false)
        else curCtx
      case _ => curCtx

    def computeNullable(given Context): tree.type =
      def setExcluded(ifTrue: Excluded, ifFalse: Excluded) =
        tree.putAttachment(CondExcluded, EitherExcluded(ifTrue, ifFalse))
      if !curCtx.isAfterTyper then tree match
        case CompareNull(x, isEqual) =>
          x.tpe match
            case ref: TermRef if ref.isStable =>
              if isEqual then setExcluded(Set(), Set(ref))
              else setExcluded(Set(ref), Set())
            case _ =>
        case Apply(Select(x, _), y :: Nil) =>
          val xc = x.condNotNullRefs
          val yc = y.condNotNullRefs
          if !(xc.isEmpty && yc.isEmpty) then
            if tree.symbol == defn.Boolean_&& then
              setExcluded(xc.ifTrue | yc.ifTrue, xc.ifFalse & yc.ifFalse)
            else if tree.symbol == defn.Boolean_|| then
              setExcluded(xc.ifTrue & yc.ifTrue, xc.ifFalse | yc.ifFalse)
        case Apply(Select(x, _), Nil) if tree.symbol == defn.Boolean_! =>
          val xc = x.condNotNullRefs
          if !xc.isEmpty then
            setExcluded(xc.ifFalse, xc.ifTrue)
        case _ =>
      tree

end Nullables
