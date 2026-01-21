package scala
package caps

import language.experimental.captureChecking

import annotation.{experimental, compileTimeOnly, retainsCap}
import annotation.meta.*

/**
 * Base trait for classes that represent capabilities in the
 * [object-capability model](https://en.wikipedia.org/wiki/Object-capability_model).
 *
 * A capability is a value representing a permission, access right, resource or effect.
 * Capabilities are typically passed to code as parameters; they should not be global objects.
 * Often, they come with access restrictions such as scoped lifetimes or limited sharing.
 *
 * An example is the [[scala.util.boundary.Label Label]] class in [[scala.util.boundary]].
 * It represents a capability in the sense that it gives permission to [[scala.util.boundary.break break]]
 * to the enclosing boundary represented by the `Label`. It has a scoped lifetime, since breaking to
 * a `Label` after the associated `boundary` was exited gives a runtime exception.
 *
 * [[Capability]] has a formal meaning when
 * [[scala.language.experimental.captureChecking Capture Checking]]
 * is turned on.
 * But even without capture checking, extending this trait can be useful for documenting the intended purpose
 * of a class.
 *
 * Capability has exactly two subtraits: [[SharedCapability Shared]] and [[ExclusiveCapability Exclusive]].
 */
sealed trait Capability extends Any

/** A marker trait for classifier capabilities that can appear in `.only`
 *  qualifiers. Capability classes directly extending `Classifier` are treated
 *  as classifier capbilities.
 *
 * [[Classifier]] has a formal meaning when
 * [[scala.language.experimental.captureChecking Capture Checking]]
 * is turned on. It should not be used outside of capture checking.
 */
trait Classifier

/** The universal capture reference. */
@experimental
object any extends Capability

/** The universal result capture reference. */
@experimental
object fresh extends Capability

@experimental // TODO: Drop once we bootstrap with 3.8.1
val cap: any.type = any

/** Marker trait for capabilities that can be safely shared in a concurrent context.
 *
 * [[SharedCapability]] has a formal meaning when
 * [[scala.language.experimental.captureChecking Capture Checking]]
 * is turned on.
 * During separation checking, shared capabilities are not taken into account.
 */
trait SharedCapability extends Capability, Classifier

@experimental
type Shared = SharedCapability

/** Marker trait for capabilities that should only be used by one concurrent process
 *  at a given time. For example, write-access to a shared mutable buffer.
 *
 * [[ExclusiveCapability]] has a formal meaning when
 * [[scala.language.experimental.captureChecking Capture Checking]]
 * is turned on.
 * During separation checking, exclusive usage of marked capabilities will be enforced.
 */
@experimental
trait ExclusiveCapability extends Capability

@experimental
type Exclusive = ExclusiveCapability

/** Marker trait for capabilities that capture some continuation or return point in
 *  the stack. Examples are exceptions, [[scala.util.boundary.Label labels]], [[scala.CanThrow CanThrow]]
 *  or Async contexts.
 *
 * [[Control]] has a formal meaning when
 * [[scala.language.experimental.captureChecking Capture Checking]]
 * is turned on.
 */
trait Control extends SharedCapability, Classifier

/** Marker trait for classes that can consult and change the global program state.
 *  These  classes typically contain mutable variables and/or update methods.
 */
@experimental
trait Stateful extends ExclusiveCapability

/** Marker trait for classes that produce fresh capabilities with their values. If a value of a type
 *  extending Separate is created, a fresh `any` is automatically added to the value's capture set.
 */
@experimental
trait Separate extends Stateful

/** Marker trait for classes that are not subject to scoping restrictions of captured capabilities.
 */
@experimental
trait Unscoped extends Stateful, Classifier

@experimental
trait Mutable extends Stateful, Separate, Unscoped

/** Marker trait for classes with reader methods, typically extended by Mutable classes. */
@experimental
@deprecated
trait Read extends Mutable


/** Carrier trait for capture set type parameters. */
@experimental
trait CapSet extends Any

/** A type constraint expressing that the capture set `C` needs to contain
 *  the capability `R`
 */
@experimental
sealed trait Contains[+C >: CapSet <: CapSet @retainsCap, R <: Singleton]

@experimental
object Contains:
  /** The only implementation of `Contains`. The constraint that `{R} <: C` is
   *  added separately by the capture checker.
   */
  @experimental
  given containsImpl[C >: CapSet <: CapSet @retainsCap, R <: Singleton]: Contains[C, R]()

/** An annotation on capset parameters `C` stating that the method's body does not
 *  have `C` in its use-set. `C` might be accessed under a box in the method
 *  or in the result type of the method. Consequently, when calling the method
 *  we do not need to charge the capture set of the actual argiment to the
 *  environment.
 *
 *  Note: This should go into annotations. For now it is here, so that we
 *  can experiment with it quickly between minor releases
 */
@experimental
final class reserve extends annotation.StaticAnnotation

/** Allowed only for source versions up to 3.7:
 *  An annotation on parameters `x` stating that the method's body makes
 *  use of the reach capability `x*`. Consequently, when calling the method
 *  we need to charge the deep capture set of the actual argiment to the
 *  environment.
 */
@experimental
@deprecated(since = "3.8.0")
final class use extends annotation.StaticAnnotation

/** A trait that used to allow expressing existential types. Replaced by
*  root.Result instances.
*/
@experimental
@deprecated
sealed trait Exists extends Capability

@experimental
object internal:

  /** An annotation to reflect that a parameter or method carries the `consume`
   *  soft modifier.
   */
  @getter @param
  final class consume extends annotation.StaticAnnotation

  /** An internal annotation placed on a refinement created by capture checking.
   *  Refinements with this annotation unconditionally override any
   *  info from the parent type, so no intersection needs to be formed.
   *  This could be useful for tracked parameters as well.
   */
  @deprecated
  final class refineOverride extends annotation.StaticAnnotation

  /** An annotation used internally for root capability wrappers of `any` that
   *  represent either Fresh or Result capabilities.
   *  A capability is encoded as `caps.any @rootCapability(...)` where
   *  `rootCapability(...)` is a special kind of annotation of type `root.Annot`
   *  that contains either a hidden set for Fresh instances or a method type binder
   *  for Result instances.
   */
  final class rootCapability extends annotation.StaticAnnotation

  /** An annotation used internally to mark a function type that was
   *  converted to a dependent function type during setup of inferred types.
   *  Such function types should not map roots to result variables.
   */
  final class inferredDepFun extends annotation.StaticAnnotation

  /** An erasedValue issued internally by the compiler. Unlike the
   *  user-accessible compiletime.erasedValue, this version is assumed
   *  to be a pure expression, hence capability safe. The compiler generates this
   *  version only where it is known that a value can be generated.
   */
  def erasedValue[T]: T = ???

  /** A trait for capabilities representing usage of mutable vars in capture sets.
   */
  trait Var[T] extends Mutable:
    def get: T
    update def set(x: T): Unit

end internal

/** A wrapper that strips all covariant capture sets from Mutable types in the
 *  result of pure operation `op`, turning them into immutable types.
 *  Array[?] is also included since it counts as a Mutable type for
 *  separation checking.
 */
@experimental
def freeze(@internal.consume x: Mutable | Array[?]): x.type = x

@experimental
object unsafe:
  /** Three usages:
   *
   *   1. Marks the constructor parameter as untracked.
   *      The capture set of this parameter will not be included in
   *      the capture set of the constructed object.
   *
   *   2. Marks a class field that has a root capability in its capture set, so
   *      that the root capability is not contributed to the class instance.
   *      Example:
   *
   *          class A { val b B^ = ... }; new A()
   *
   *      has type A^ since b contributes an `any`. But
   *
   *          class A { @untrackedCaptures val b: B^ = ... }; new A()
   *
   *      has type A. The `b` field does not contribute its `any`.
   *
   *   3. Allows a field to be declarewd in a class that does not extend Stateful,
   *      and suppresses checks for updates to the field.
   *
   * @note This should go into annotations. For now it is here, so that we
   *  can experiment with it quickly between minor releases
   */
  @getter @param
  final class untrackedCaptures extends annotation.StaticAnnotation

  extension [T](x: T)
    /** A specific cast operation to remove a capture set.
     *  If argument is of type `T^C`, assume it is of type `T` instead.
     *  Calls to this method are treated specially by the capture checker.
     */
    def unsafeAssumePure: T = x

  /** A wrapper around code for which separation checks are suppressed.
   */
  def unsafeAssumeSeparate(op: Any): op.type = op

  /** A wrapper around code for which uses go unrecorded. */
  def unsafeDiscardUses(op: Any): op.type = op

  /** An unsafe variant of erasedValue that can be used as an escape hatch. Unlike the
   *  user-accessible compiletime.erasedValue, this version is assumed
   *  to be a pure expression, hence capability safe. But there is no proof
   *  of realizability, hence it is unsafe.
   */
  def unsafeErasedValue[T]: T = ???

end unsafe
