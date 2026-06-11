package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Hashable.Binders
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{CachedProxyType, SingletonType, Type}

/** A reference inside an [[ENode]] — one of three kinds, each modelled as
 *  a case of this sealed hierarchy:
 *  - [[ENodeVar.BoundParam]]: de Bruijn index bound by an enclosing
 *    [[ENode.Lambda]].
 *  - [[ENodeVar.OpenedParam]]: free variable introduced when "opening" a
 *    lambda during implication checking (akin to the opening operation in a
 *    locally-nameless representation). Scoped to a single `implies` call.
 *  - [[ENodeVar.Skolem]]: a skolem standing for an opaque value — the
 *    unstable argument of a dependent function (allocated by
 *    [[QualifiedTypes.substParamInQualifiers]] and persisted across
 *    re-type-checks), or a `SkolemType` / non-singleton demoted by
 *    [[ENode.singleton]].
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

  /** A skolem for the unstable argument of a dependent function, allocated
   *  by [[QualifiedTypes.substParamInQualifiers]] and persisted via a
   *  `@QualifierSkolemIndex` annotation on the argument tree (or the
   *  EtaExpansion-lifted symbol) so its identity is preserved across
   *  re-type-checks. Also used by [[ENode.singleton]] to demote a
   *  `SkolemType` or non-singleton type to an opaque atom.
   *
   *  Identity is `(owner, index)`: each owner symbol has its own per-owner
   *  index space, so two skolems with the same `index` but different
   *  owners are distinct. This makes indices stable across TASTy reads
   *  (owner symbols are stable in TASTy by design) and yields per-function
   *  small indices in error messages.
   */
  final case class Skolem(owner: Symbol, index: Int)(_underlying: Type) extends ENodeVar(_underlying):
    override def computeHash(bs: Binders): Int = doHash(2, owner.id * 41 + index)
    def derivedENodeVar(underlying: Type): ENodeVar =
      if underlying eq _underlying then this else Skolem(owner, index)(underlying)
