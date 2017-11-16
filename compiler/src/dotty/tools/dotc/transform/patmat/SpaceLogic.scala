package dotty.tools.dotc.transform.patmat

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.Type

/** Space logic for checking exhaustivity and unreachability of pattern matching.
 *
 *  Space can be thought of as a set of possible values. A type or a pattern
 *  both refer to spaces. The space of a type is the values that inhabit the
 *  type. The space of a pattern is the values that can be covered by the
 *  pattern.
 *
 *  Space is recursively defined as follows:
 *
 *      1. For a type T, `Typ(T)` is a space
 *      2. `Prod(S1, S2, ..., Sn)` is a product space.
 *
 *  To correctly handle GADTs, we think in terms of _constrained_ spaces.
 *  A constrained space containst a list of spaces and represents a set of _vectors_
 *  of values. This would be a natural representation if we could pattern match on multiple values.
 *  Additionally, as the name suggests, a constrained space can have _constraints_,
 *  both term and type-level. The constraints are simply predicates restricting the value vectors
 *  contained in the set represented by a constrained space.
 *  Finally, a list of constrained spaces is used exactly like a union would.
 *
 *  For the problem of exhaustivity check, its formulation in terms of space is as follows:
 *
 *      Is the space Typ(T) a subspace of the union of spaces covered by all the patterns?
 *
 *  The problem of unreachable patterns can be formulated as follows:
 *
 *      Is the space covered by a pattern a subspace of the spaces covered by previous patterns?
 *
 *  Assumption:
 *    (1) One case class cannot be inherited directly or indirectly by another
 *        case class.
 *    (2) Inheritance of a case class cannot be well handled by the algorithm.
 *
 */
trait SpaceLogic {
  /** Display space in string format */
  def show(spaces: List[Space]): String

  def debugShow(s: ConstrainedSpace): String

  def show(a: ConstrainedSpace): String = {
    val shownVec = show(a.vec)
    if (a.vec.length == 1) shownVec else s"[$shownVec]"
  }

  def show(as: Seq[ConstrainedSpace]): String =
    as.map(show).mkString("{", ", ", "}")

  def debugShow(as: Seq[ConstrainedSpace]): String =
    as.map(debugShow).mkString("{", ", ", "}")

  /** Run `thunk` in a debugging block, indenting all messages */
  def doDebug[T](pre: => String, post: T => String)(thunk: => T): T

  /** Is `tp1` a subtype of `tp2`? */
  def isSubType(tp1: Type, tp2: Type): Boolean

  /** Is `tp1` the same type as `tp2`? */
  def isEqualType(tp1: Type, tp2: Type): Boolean

  /** Return a space containing the values of both types.
   *
   * The types should be atomic (non-decomposable) and unrelated (neither
   * should be a subtype of the other).
   */
  def intersectUnrelatedAtomicTypes(tp1: Type, tp2: Type): Option[Space]

  /** Is the type `tp` decomposable? i.e. all values of the type can be covered
   * by its decomposed types.
   *
   * Abstract sealed class, OrType, Boolean and Java enums can be decomposed.
   */
  def canDecompose(tp: Type): Boolean

  /** Return term parameter types of the extractor `unapp` */
  def signature(unapp: Type, unappSym: Symbol, argLen: Int): List[Type]

  /** Get components of decomposable types */
  def decompose(a: ConstrainedSpace): List[ConstrainedSpace]

  /** Intersection of two spaces */
  def intersect(a: ConstrainedSpace, b: ConstrainedSpace): List[ConstrainedSpace] =
  doDebug[List[ConstrainedSpace]](s"${debugShow(a)} intersect ${debugShow(b)}", res => s"= ${debugShow(res)}") {
    def doDecomposeA = decompose(a).flatMap(intersect(_, b))
    def doDecomposeB = decompose(b).flatMap(intersect(a, _))

    val res: List[ConstrainedSpace] = (a.vec, b.vec) match {
      case (Nil, Nil) =>
        // TODO: optimize this?
        def union[T](l1: List[T], l2: List[T]) =
          (l1 ::: l2).distinct

        List(ConstrainedSpace(Nil, union(a.termConstraints, b.termConstraints), union(a.typeConstraints, b.typeConstraints)))

      case (n :: ns, m :: ms) => (n, m) match {
        case (nprod@Prod(_, nfun, nsym, nss, _), Prod(_, mfun, msym, mss, _)) =>
          if (nsym != msym || !isEqualType(nfun, mfun)) Nil
          else {
            val arity = nss.length // assuming arity == mps.length
            intersect(a withVec (nss ::: ns), b withVec (mss ::: ms)).map { c =>
              val (kps, rest) = c.vec.splitAt(arity)
              c.withVec(nprod.copy(params = kps) :: rest)
            }
          }

        case (Prod(tp1, _, _, _, _), Typ(tp2, _)) =>
          if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) List(a)
          else if (canDecompose(tp2)) doDecomposeB
          else Nil

        case (Typ(tp1, _), Prod(tp2, _, _, _, _)) =>
          if (isSubType(tp1, tp2) || isSubType(tp2, tp1)) List(b)
          else if (canDecompose(tp1)) doDecomposeA
          else Nil

        case (Typ(tp1, _), Typ(tp2, _)) =>
          if (isSubType(tp1, tp2)) List(a)
          else if (isSubType(tp2, tp1)) List(b)
          else if (canDecompose(tp1)) doDecomposeA
          else if (canDecompose(tp2)) doDecomposeB
          else intersectUnrelatedAtomicTypes(tp1, tp2) match {
            case None => Nil
            case Some(space) =>
              intersect(a.withVec(ns), b.withVec(ms)).map {
                c => c.withVec(space :: c.vec)
              }
          }

        case _ => Nil
      }
      case _ => Nil
    }

    res
  }

  /** The space of a not covered by b */
  def subtract(a: ConstrainedSpace, b: ConstrainedSpace): List[ConstrainedSpace] =
  doDebug[List[ConstrainedSpace]](s"${debugShow(a)} substract ${debugShow(b)}", res => s"= ${debugShow(res)}") {
    def doDecomposeA = decompose(a).flatMap(subtract(_, b))
    def doDecomposeB = decompose(b).foldLeft(List(a)) { (as, b) => as.flatMap(subtract(_, b)) }

    val res = (a.vec, b.vec) match {
      case (Nil, Nil) =>
        if (b.termConstraints.isEmpty) Nil
        else b.termConstraints.map { d => a.withTermConstraints(d.neg :: a.termConstraints) }

      case (n :: ns, m :: ms) => (n, m) match {
        case (nprod@Prod(_, nfun, nsym, nss, _), Prod(_, mfun, msym, mss, _)) =>
          if (nsym != msym || !isEqualType(nfun, mfun)) List(a)
          else {
            val arity = nss.length // assuming nss.length == mss.length
            subtract(a withVec (nss ::: ns), b withVec (mss ::: ms)).map { c =>
              val (kps, rest) = c.vec.splitAt(arity)
              c.withVec(nprod.copy(params = kps) :: rest)
            }
          }

        case (p@Prod(tp1, _, _, _, full), Typ(tp2, _)) =>
          if (isSubType(tp1, tp2)) {
            val tailSubtraction = subtract(a withVec ns, b withVec ms)
            tailSubtraction.map(_.withPrependendSpace(p))
          } else if (full && canDecompose(tp2)) doDecomposeB
          else List(a)

        case (Typ(_, _), Prod(_, _, _, _, false)) =>
          List(a) // approximation

        case (Typ(tp1, _), p@Prod(tp2, fun, sym, ss, true)) =>
          if (isSubType(tp1, tp2)) {
            val newParams = signature(fun, sym, ss.length).map(Typ(_))
            val _a = a.withVec(p.copy(tp = tp2, params = newParams) :: ns)
            subtract(_a, b)
          } else if (canDecompose(tp1)) doDecomposeA
          else List(a)

        case (Typ(tp1, _), Typ(tp2, _)) =>
          if (isSubType(tp1, tp2)) {
            val tailSubtraction = subtract(a withVec ns, b withVec ms)
            tailSubtraction.map(_.withPrependendSpace(Typ(tp1)))
          } else if (canDecompose(tp1)) doDecomposeA
          else if (canDecompose(tp2)) doDecomposeB
          else List(a)

        case _ => List(a)
      }
      case _ => List(a)
    }

    res
  }
}
