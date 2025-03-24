package scala
package caps

import annotation.{experimental, compileTimeOnly, retainsCap}

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
 */
@experimental
trait Capability extends Any

/** The universal capture reference. */
@experimental
object cap extends Capability

/** Carrier trait for capture set type parameters */
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

/** An annotation on parameters `x` stating that the method's body makes
 *  use of the reach capability `x*`. Consequently, when calling the method
 *  we need to charge the deep capture set of the actual argiment to the
 *  environment.
 *
 *  Note: This should go into annotations. For now it is here, so that we
 *  can experiment with it quickly between minor releases
 */
@experimental
final class use extends annotation.StaticAnnotation

/** A trait to allow expressing existential types such as
 *
 *      (x: Exists) => A ->{x} B
 */
@experimental
sealed trait Exists extends Capability

@experimental
object internal:

  /** A wrapper indicating a type variable in a capture argument list of a
   *  @retains annotation. E.g. `^{x, Y^}` is represented as `@retains(x, capsOf[Y])`.
   */
  @compileTimeOnly("Should be be used only internally by the Scala compiler")
  def capsOf[CS >: CapSet <: CapSet @retainsCap]: Any = ???

  /** Reach capabilities x* which appear as terms in @retains annotations are encoded
   *  as `caps.reachCapability(x)`. When converted to CaptureRef types in capture sets
   *  they are  represented as `x.type @annotation.internal.reachCapability`.
   */
  extension (x: Any) def reachCapability: Any = x

@experimental
object unsafe:
  /**
   * Marks the constructor parameter as untracked.
   * The capture set of this parameter will not be included in
   * the capture set of the constructed object.
   *
   * @note This should go into annotations. For now it is here, so that we
   *  can experiment with it quickly between minor releases
   */
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

end unsafe
