package scala

import annotation.{experimental, compileTimeOnly, retainsCap}

@experimental object caps:

  trait Capability extends Any

  /** The universal capture reference */
  val cap: Capability = new Capability() {}

  /** The universal capture reference (deprecated) */
  @deprecated("Use `cap` instead")
  val `*`: Capability = cap

  @deprecated("Use `Capability` instead")
  type Cap = Capability

  trait Mutable extends Capability

  /** Carrier trait for capture set type parameters */
  trait CapSet extends Any

  /** A type constraint expressing that the capture set `C` needs to contain
   *  the capability `R`
   */
  sealed trait Contains[+C >: CapSet <: CapSet @retainsCap, R <: Singleton]

  /** The only implementation of `Contains`. The constraint that `{R} <: C` is
   *  added separately by the capture checker.
   */
  given containsImpl[C >: CapSet <: CapSet @retainsCap, R <: Singleton]: Contains[C, R]()

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

  /** Read-only capabilities x.rd which appear as terms in @retains annotations are encoded
   *  as `caps.readOnlyCapability(x)`. When converted to CaptureRef types in capture sets
   *  they are  represented as `x.type @annotation.internal.readOnlyCapability`.
   */
  extension (x: Any) def readOnlyCapability: Any = x

  /** A trait to allow expressing existential types such as
   *
   *      (x: Exists) => A ->{x} B
   */
  sealed trait Exists extends Capability

  /** This should go into annotations. For now it is here, so that we
   *  can experiment with it quickly between minor releases
   */
  final class untrackedCaptures extends annotation.StaticAnnotation

  /** An annotation on parameters `x` stating that the method's body makes
   *  use of the reach capability `x*`. Consequently, when calling the method
   *  we need to charge the deep capture set of the actual argiment to the
   *  environment.
   *
   *  Note: This should go into annotations. For now it is here, so that we
   *  can experiment with it quickly between minor releases
   */
  final class use extends annotation.StaticAnnotation

  object unsafe:

    extension [T](x: T)
      /** A specific cast operation to remove a capture set.
       *  If argument is of type `T^C`, assume it is of type `T` instead.
       *  Calls to this method are treated specially by the capture checker.
       */
      def unsafeAssumePure: T = x

    /** A wrapper around code for which separation checks are suppressed.
     */
    def unsafeAssumeSeparate[T](op: T): T = op

  end unsafe
end caps