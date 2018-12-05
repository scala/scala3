package dotty.tools.dotc.core

import dotty.tools.dotc.ast.Trees.{Apply, Literal, Select}
import dotty.tools.dotc.ast.tpd._
import StdNames.nme
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.{NonNullTermRef, TermRef, Type}

import scala.annotation.internal.sharable

/** Operations on flow-sensitive type information
 *
 *  Currently the following idioms are supported:
 *    (1)
 *    ```
 *    val x: String|Null = "foo"
 *    if (x != null) {
 *      // x: String in the "then" branch
 *    }
 *    ```
 *    Notice that `x` must be stable for the above to work.
 */
object FlowFacts {

  /** A set of `TermRef`s known to be non-nullable at the current program point */
  type NonNullSet = Set[TermRef]

  /** The initial state where no `TermRef`s are known to be non-null */
  @sharable val emptyNonNullSet = Set.empty[TermRef]

  /** Is `tref` non-null (even if its info says it isn't)? */
  def isNonNull(nnSet: NonNullSet, tref: TermRef): Boolean = {
    nnSet.contains(tref)
  }

  /** Tries to improve de "precision" of `tpe` using flow-sensitive type information. */
  def refineType(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case tref: TermRef if isNonNull(ctx.nonNullFacts, tref) =>
      NonNullTermRef.fromTermRef(tref)
    case _ => tpe
  }

  /** Analyze the control statement in `tree` to learn new facts about non-nullability. */
  def inferNonNull(tree: Tree)(implicit ctx: Context): NonNullSet = {
    tree match {
      case Apply(Select(lhs, nme.NE), List(Literal(const))) if const.tag == Constants.NullTag =>
        lhs.tpe match {
          case tref: TermRef if tref.isStable => Set(tref)
          case _ => emptyNonNullSet
        }
      case _ => emptyNonNullSet
    }
  }
}
