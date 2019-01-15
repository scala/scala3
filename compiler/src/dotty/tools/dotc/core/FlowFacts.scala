package dotty.tools.dotc.core

import dotty.tools.dotc.ast.Trees.{Apply, Literal, Select}
import dotty.tools.dotc.ast.tpd._
import StdNames.nme
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types.{NonNullTermRef, TermRef, Type}

import scala.annotation.internal.sharable

/** Operations on flow-sensitive type information */
object FlowFacts {

  /** A set of `TermRef`s known to be non-null at the current program point */
  type NonNullSet = Set[TermRef]

  /** The initial state where no `TermRef`s are known to be non-null */
  @sharable val emptyNonNullSet = Set.empty[TermRef]

  /** Is `tref` non-null (even if its info says it isn't)? */
  def isNonNull(nnSet: NonNullSet, tref: TermRef): Boolean = {
    nnSet.contains(tref)
  }

  /** Try to improve the precision of `tpe` using flow-sensitive type information. */
  def refineType(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case tref: TermRef if isNonNull(ctx.nonNullFacts, tref) =>
      NonNullTermRef.fromTermRef(tref)
    case _ => tpe
  }

  /** Nullability facts inferred from a condition.
   *  @param ifTrue are the terms known to be non-null if the condition is true.
   *  @param ifFalse are the terms known to be non-null if the condition is false.
   */
  case class Inferred(ifTrue: NonNullSet, ifFalse: NonNullSet) {
    // Let `NN(e, true/false)` be the set of terms that are non-null if `e` evaluates to `true/false`.
    // We can use De Morgan's laws to underapproximate `NN` via `Inferred`.
    // e.g. say `e = e1 && e2`. Then if `e` is `false`, we know that either `!e1` or `!e2`.
    // Let `t` be a term that is in both `NN(e1, false)` and `NN(e2, false)`.
    // Then it follows that `t` must be in `NN(e, false)`. This means that if we set
    // `Inferred(e1 && e2, false) = Inferred(e1, false) ∩ Inferred(e2, false)`, we'll have
    // `Inferred(e1 && e2, false) ⊂ NN(e1 && e2, false)` (formally, we'd do a structural induction on `e`).
    // This means that when we infer something we do so soundly. The methods below use this approach.

    /** If `this` corresponds to a condition `e1` and `other` to `e2`, calculate the inferred facts for `e1 && e2`. */
    def combineAnd(other: Inferred): Inferred = Inferred(ifTrue.union(other.ifTrue), ifFalse.intersect(other.ifFalse))

    /** If `this` corresponds to a condition `e1` and `other` to `e2`, calculate the inferred facts for `e1 || e2`. */
    def combineOr(other: Inferred): Inferred = Inferred(ifTrue.intersect(other.ifTrue), ifFalse.union(other.ifFalse))

    /** The inferred facts for the negation of this condition. */
    def negate: Inferred = Inferred(ifFalse, ifTrue)
  }

  object Inferred {
    /** Create a singleton inferred fact containing `tref`. */
    def apply(tref: TermRef, ifTrue: Boolean): Inferred = {
      if (ifTrue) Inferred(Set(tref), emptyNonNullSet)
      else Inferred(emptyNonNullSet, Set(tref))
    }
  }

  /** Analyze the tree for a condition `cond` to learn new facts about non-nullability.
   *  Supports ands, ors, and unary negation.
   *
   *  Example:
   *  (1)
   *  ```
   *  val x: String|Null = "foo"
   *  if (x != null) {
   *    // x: String in the "then" branch
   *  }
   *  ```
   * Notice that `x` must be stable for the above to work.
   *
   * TODO(abeln): add longer description of the algorithm
   */
  def inferNonNull(cond: Tree)(implicit ctx: Context): Inferred = {
    val emptyFacts = Inferred(emptyNonNullSet, emptyNonNullSet)

    /** Combine two sets of facts according to `op`. */
    def combine(lhs: Inferred, op: Name, rhs: Inferred): Inferred = {
      op match {
        case _ if op == nme.ZAND => lhs.combineAnd(rhs)
        case _ if op == nme.ZOR => lhs.combineOr(rhs)
      }
    }

    /** Recurse over a conditional to extract flow facts. */
    def recur(tree: Tree): Inferred = {
      tree match {
        case Apply(Select(lhs, op), List(rhs)) =>
          if (op == nme.ZAND || op == nme.ZOR) combine(recur(lhs), op, recur(rhs))
          else if (op == nme.EQ || op == nme.NE) newFact(lhs, isEq = op == nme.EQ, rhs)
          else emptyFacts
        case Select(lhs, neg) if neg == nme.UNARY_! =>
          recur(lhs).negate
        case _ => emptyFacts
      }
    }

    /** Extract new facts from an expression `lhs = rhs` or `lhs != rhs`
     *  if either the lhs or rhs is the `null` literal.
     */
    def newFact(lhs: Tree, isEq: Boolean, rhs: Tree): Inferred = {
      def isNullLit(tree: Tree): Boolean = tree match {
        case Literal(const) if const.tag == Constants.NullTag => true
        case _ => false
      }

      def isStableTermRef(tree: Tree): Boolean = asStableTermRef(tree).isDefined

      def asStableTermRef(tree: Tree): Option[TermRef] = tree.tpe match {
        case tref: TermRef if tref.isStable => Some(tref)
        case _ => None
      }

      val trefOpt =
        if (isNullLit(lhs) && isStableTermRef(rhs)) asStableTermRef(rhs)
        else if (isStableTermRef(lhs) && isNullLit(rhs)) asStableTermRef(lhs)
        else None

      trefOpt match {
        case Some(tref) =>
          // If `isEq`, then the condition is of the form `lhs == null`,
          // in which case we know `lhs` is non-null if the condition is false.
          Inferred(tref, ifTrue = !isEq)
        case _ => emptyFacts
      }
    }

    recur(cond)
  }

  /** Propagate flow-sensitive type information inside a condition.
   *  Specifically, if `cond` is of the form `lhs &&` or `lhs ||`, where the lhs has already been typed
   *  (and the rhs hasn't been typed yet), compute the non-nullability info we get from lhs and
   *  return a new context with it. The new context can then be used to type the rhs.
   *
   *  This is useful in e.g.
   *  ```
   *  val x: String|Null = ???
   *  if (x != null && x.length > 0) ...
   *  ```
   */
  def propagateWithinCond(cond: Tree)(implicit ctx: Context): Context = {
    cond match {
      case Select(lhs, op) if op == nme.ZAND || op == nme.ZOR =>
        val Inferred(ifTrue, ifFalse) = FlowFacts.inferNonNull(lhs)
        if (op == nme.ZAND) ctx.fresh.addNonNullFacts(ifTrue)
        else ctx.fresh.addNonNullFacts(ifFalse)
      case _ => ctx
    }
  }
}
