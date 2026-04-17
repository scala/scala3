package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Types.{CachedProxyType, SingletonType, Type}

/** The kind of an [[ENodeVar]] — its semantic role in the ENode structure. */
enum ENodeVarKind:
  /** A de Bruijn index bound by an enclosing [[ENode.Lambda]]. */
  case BoundParam

  /** A free variable introduced when "opening" a lambda during implication
   *  checking (akin to the opening operation in a locally-nameless
   *  representation). Scoped to a single `implies` call.
   */
  case OpenedParam

  /** A skolem for an argument reference, allocated by
   *  [[QualifiedTypes.substParamInQualifiers]] and persisted in a sticky
   *  attachment on the argument tree so its identity is preserved across
   *  re-type-checks (typer / posttyper / Ycheck).
   */
  case Skolem

/** A reference inside an [[ENode]] — either a de Bruijn bound parameter, a
 *  free variable from lambda opening, or an argument-reference skolem.
 *
 *  Identity (and hash) is determined by `(kind, index)` only; the
 *  `underlying` type is an informational hint that may be transformed by
 *  generic `TypeMap`s (e.g., avoidance) without affecting equality.
 *
 *  @param kind The kind of variable — see [[ENodeVarKind]]
 *  @param index For [[ENodeVarKind.BoundParam]]: de Bruijn index (0 = innermost).
 *               For [[ENodeVarKind.OpenedParam]] and [[ENodeVarKind.Skolem]]: a
 *               unique identifier within the relevant scope.
 *  @param _underlying Underlying type of the variable
 */
final case class ENodeVar(kind: ENodeVarKind, index: Int)(_underlying: Type) extends CachedProxyType, SingletonType:
  override def underlying(using Context): Type = _underlying

  /** The underlying type without requiring a Context. Used when we need to
   *  inspect the type for printing or for deriving a new [[ENodeVar]].
   */
  def rawUnderlying: Type = _underlying

  override def computeHash(bs: Binders): Int = doHash(kind.ordinal, index)
  override def equals(that: Any): Boolean = that match
    case that: ENodeVar => this.kind == that.kind && this.index == that.index
    case _ => false

  def derivedENodeVar(kind: ENodeVarKind, index: Int, underlying: Type): ENodeVar =
    if kind == this.kind && index == this.index && (underlying eq _underlying) then this
    else ENodeVar(kind, index)(underlying)

  /** True iff this is a free variable (not a de Bruijn bound param). */
  def isFree: Boolean = kind match
    case ENodeVarKind.BoundParam => false
    case ENodeVarKind.OpenedParam | ENodeVarKind.Skolem => true
