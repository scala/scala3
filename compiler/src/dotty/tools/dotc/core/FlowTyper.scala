package dotty.tools.dotc.core

import dotty.tools.dotc.ast.tpd._
import StdNames.nme
import dotty.tools.dotc.ast.Trees.{Apply, Block, If, Select, Ident}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Types.{NonNullTermRef, TermRef, Type}

import scala.annotation.internal.sharable

/** Flow-sensitive typer */
object FlowTyper {

  /** A set of `TermRef`s known to be non-null at the current program point */
  type FlowFacts = Set[TermRef]

  /** The initial state where no `TermRef`s are known to be non-null */
  val emptyFlowFacts = Set.empty[TermRef]

  /** Tries to improve the precision of `tpe` using flow-sensitive type information.
   *  For nullability, is `tpe` is a `TermRef` declared as nullable but known to be non-nullable because of the
   *  contextual info, returns the non-nullable version of the type.
   *  If the precision of the type can't be improved, then returns the type unchanged.
   */
  def refineType(tpe: Type)(implicit ctx: Context): Type = {
    assert(ctx.explicitNulls)
    tpe match {
      case tref: TermRef if ctx.flowFacts.contains(tref) => NonNullTermRef(tref.prefix, tref.designator)
      case _ => tpe
    }
  }

  /** Nullability facts inferred from a condition.
   *  @param ifTrue are the terms known to be non-null if the condition is true.
   *  @param ifFalse are the terms known to be non-null if the condition is false.
   */
  case class Inferred(ifTrue: FlowFacts, ifFalse: FlowFacts) {
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

  /** Analyze the tree for a condition `cond` to learn new flow facts.
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
   * Let NN(cond, true/false) be the set of paths (`TermRef`s) that we can infer to be non-null
   * if `cond` is true/false, respectively. Then define NN by (basically De Morgan's laws):
   *
   *   NN(p == null, true)  = {}                  we also handle `eq`
   *   NN(p == null, false) = {p} if p is stable
   *   NN(p != null, true)  = {p} if p is stable  we also handle `ne`
   *   NN(p != null, false) = {}
   *   NN(p.isInstanceOf[Null], true) = {}
   *   NN(p.isInstanceOf[Null], false) = {p} if p is stable
   *   NN(A && B,    true)  = ∪(NN(A, true),  NN(B, true))
   *   NN(A && B,    false) = ∩(NN(A, false), NN(B, false))
   *   NN(A || B,    true)  = ∩(NN(A, true),  NN(B, true))
   *   NN(A || B,    false) = ∪(NN(A, false), NN(B, false))
   *   NN(!A,        true)  = NN(A, false)
   *   NN(!A,        false) = NN(A, true)
   *   NN({S1; ...; Sn, cond}, true/false) = NN(cond, true/false)
   *   NN(cond,      _)     = {} otherwise
   */
  def inferFromCond(cond: Tree)(implicit ctx: Context): Inferred = {
    assert(ctx.explicitNulls)
    /** Combine two sets of facts according to `op`. */
    def combine(lhs: Inferred, op: Name, rhs: Inferred): Inferred = {
      op match {
        case _ if op == nme.ZAND => lhs.combineAnd(rhs)
        case _ if op == nme.ZOR => lhs.combineOr(rhs)
      }
    }

    val emptyFacts = Inferred(emptyFlowFacts, emptyFlowFacts)
    val nullLit = tpd.Literal(Constant(null))

    /** Recurse over a conditional to extract flow facts. */
    def recur(tree: Tree): Inferred = {
      tree match {
        // `==` and `!=` are methods in `Any`, so `x == y` gets desugared to
        // `x.==(y)`
        case Apply(Select(lhs, op), List(rhs)) =>
          if (op == nme.ZAND || op == nme.ZOR) combine(recur(lhs), op, recur(rhs))
          else if (op == nme.EQ || op == nme.NE) newFact(lhs, isEq = (op == nme.EQ), rhs)
          else emptyFacts
        // `eq` and `ne` are extension methods if the receiver is a nullable union,
        // so `(x: String|Null) eq null` gets desugared to `eq(x)(null)`.
        case Apply(Apply(Ident(op), List(lhs)), List(rhs)) =>
          if (op == nme.eq || op == nme.ne) newFact(lhs, isEq = (op == nme.eq), rhs)
          else emptyFacts
        // TODO(abeln): handle type test with argument that's not a subtype of `Null`.
        // We could infer "non-null" in that case: e.g. `if (x.isInstanceOf[String]) { // x can't be null }`
        // case TypeApply(Select(lhs, op), List(tArg)) if op == nme.isInstanceOf_ && tArg.tpe.isNullType =>
        //  newFact(lhs, isEq = true, nullLit)
        case Select(lhs, op) if op == nme.UNARY_! => recur(lhs).negate
        case Block(_, expr) => recur(expr)
        case inline: Inlined => recur(inline.expansion)
        case typed: Typed => recur(typed.expr) // TODO(abeln): check that the type is `Boolean`?
        case _ => emptyFacts
      }
    }

    /** Extract new facts from an expression `lhs = rhs` or `lhs != rhs`
     *  if either the lhs or rhs is the `null` literal.
     */
    def newFact(lhs: Tree, isEq: Boolean, rhs: Tree): Inferred = {
      def isNullLit(tree: Tree): Boolean = tree match {
        case lit: Literal if lit.const.tag == Constants.NullTag => true
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
          if (!isEq) Inferred(Set(tref), emptyFlowFacts)
          else Inferred(emptyFlowFacts, Set(tref))
        case _ => emptyFacts
      }
    }

    recur(cond)
  }

  /** Infer flow-sensitive type information inside a condition.
   *
   *  Specifically, if `cond` is of the form `lhs &&` or `lhs ||`, where the lhs has already been typed
   *  (and the rhs hasn't been typed yet), compute the non-null facts that must hold so that the rhs can
   *  execute. These facts can then be soundly assumed when typing the rhs, because boolean operators are
   *  short-circuiting.
   *
   *  This is useful in e.g.
   *  ```
   *  val x: String|Null = ???
   *  if (x != null && x.length > 0) ...
   *  ```
   */
  def inferWithinCond(cond: Tree)(implicit ctx: Context): FlowFacts = {
    assert(ctx.explicitNulls)
    cond match {
      case Select(lhs, op) if op == nme.ZAND || op == nme.ZOR =>
        val Inferred(ifTrue, ifFalse) = inferFromCond(lhs)
        if (op == nme.ZAND) ifTrue
        else ifFalse
      case _ => emptyFlowFacts
    }
  }

  /** Infer flow-sensitive type information within a block.
   *
   *  More precisely, if `s1; s2` are consecutive statements in a block, this returns
   *  a context with nullability facts that hold once `s1` has executed.
   *  The new facts can then be used to type `s2`.
   *
   *  This is useful for e.g.
   *  ```
   *  val x: String|Null = ???
   *  if (x == null) return "foo"
   *  val y = x.length // x: String inferred
   *  ```
   *
   *  How can we obtain additional facts just from the fact that `s1` executed?
   *  This can happen if `s1` is of the form `If(cond, then, else)`, where `then` or
   *  `else` have non-local control flow.
   *
   *  The following qualify as non-local:
   *  1) a return
   *  2) an expression of type `Nothing` (in particular, usages of `throw`)
   *  3) a block where the last expression is non-local
   *
   *  So, for example, if we know that `x` must be non-null if `cond` is true, and `else` is non-local,
   *  then in order for `s2` to execute `cond` must be true. We can thus soundly add `x` to our
   *  flow facts.
   */
  def inferWithinBlock(stat: Tree)(implicit ctx: Context): FlowFacts = {
    def isNonLocal(s: Tree): Boolean = s match {
      case _: Return => true
      case Block(_, expr) => isNonLocal(expr)
      case _ =>
        // If the type is bottom (like the result of a `throw`), then we assume the statement
        // won't finish executing.
        s.tpe.isBottomType
    }

    assert(ctx.explicitNulls)
    stat match {
      case If(cond, thenExpr, elseExpr) =>
        val Inferred(ifTrue, ifFalse) = inferFromCond(cond)
        if (isNonLocal(thenExpr) && isNonLocal(elseExpr)) ifTrue ++ ifFalse
        else if (isNonLocal(thenExpr)) ifFalse
        else if (isNonLocal(elseExpr)) ifTrue
        else emptyFlowFacts
      case _ => emptyFlowFacts
    }
  }
}
