package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Types.{CachedProxyType, SingletonType, Type}

/** A reference inside an [[ENode]] — one of three kinds, each modelled as
 *  a case of this sealed hierarchy:
 *  - [[ENodeVar.BoundParam]]: de Bruijn index bound by an enclosing
 *    [[ENode.Lambda]].
 *  - [[ENodeVar.OpenedParam]]: free variable introduced when "opening" a
 *    lambda during implication checking (akin to the opening operation in a
 *    locally-nameless representation). Scoped to a single `implies` call.
 *  - [[ENodeVar.Skolem]]: an argument-reference skolem, allocated by
 *    [[QualifiedTypes.substParamInQualifiers]] / [[QualifiedTypes.avoidRefs]]
 *    and persisted across re-type-checks (typer / posttyper / Ycheck).
 *
 *  Identity (and hash) is determined by the case class and `index`; the
 *  `underlying` type is an informational hint that may be transformed by
 *  generic `TypeMap`s (e.g., avoidance) without affecting equality.
 */
sealed abstract class ENodeVar(_underlying: Type) extends CachedProxyType, SingletonType:
  /** For [[ENodeVar.BoundParam]]: de Bruijn index (0 = innermost).
   *  For [[ENodeVar.OpenedParam]] and [[ENodeVar.Skolem]]: a unique
   *  identifier within the relevant scope.
   */
  def index: Int

  override def underlying(using Context): Type = _underlying

  /** The underlying type without requiring a Context. Used when we need to
   *  inspect the type for printing or for deriving a new [[ENodeVar]].
   */
  def rawUnderlying: Type = _underlying

  /** Build a new [[ENodeVar]] of the same kind and index with a transformed
   *  underlying type. Used by generic `TypeMap`s.
   */
  def derivedENodeVar(underlying: Type): ENodeVar

  /** True iff this is a free variable (not a de Bruijn bound param). */
  def isFree: Boolean = this match
    case _: ENodeVar.BoundParam => false
    case _: ENodeVar.OpenedParam => true
    case _: ENodeVar.Skolem => true

object ENodeVar:
  /** A de Bruijn index bound by an enclosing [[ENode.Lambda]]. */
  final case class BoundParam(index: Int)(_underlying: Type) extends ENodeVar(_underlying):
    override def computeHash(bs: Binders): Int = doHash(0, index)
    def derivedENodeVar(underlying: Type): ENodeVar =
      if underlying eq _underlying then this else BoundParam(index)(underlying)

  /** A free variable introduced when "opening" a lambda during implication
   *  checking. Scoped to a single `implies` call.
   */
  final case class OpenedParam(index: Int)(_underlying: Type) extends ENodeVar(_underlying):
    override def computeHash(bs: Binders): Int = doHash(1, index)
    def derivedENodeVar(underlying: Type): ENodeVar =
      if underlying eq _underlying then this else OpenedParam(index)(underlying)

  /** A skolem for an argument reference, allocated by
   *  [[QualifiedTypes.substParamInQualifiers]] / [[QualifiedTypes.avoidRefs]]
   *  and persisted in a sticky attachment on the argument tree so its
   *  identity is preserved across re-type-checks.
   */
  final case class Skolem(index: Int)(_underlying: Type) extends ENodeVar(_underlying):
    override def computeHash(bs: Binders): Int = doHash(2, index)
    def derivedENodeVar(underlying: Type): ENodeVar =
      if underlying eq _underlying then this else Skolem(index)(underlying)
