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

  /** Carrier trait for capture set type parameters */
  trait CapSet extends Any

  /** A type constraint expressing that the capture set `C` needs to contain
   *  the capability `R`
   */
  sealed trait Contains[C <: CapSet @retainsCap, R <: Singleton]

  /** The only implementation of `Contains`. The constraint that `{R} <: C` is
   *  added separately by the capture checker.
   */
  given containsImpl[C <: CapSet @retainsCap, R <: Singleton]: Contains[C, R]()

  /** A wrapper indicating a type variable in a capture argument list of a
   *  @retains annotation. E.g. `^{x, Y^}` is represented as `@retains(x, capsOf[Y])`.
   */
  @compileTimeOnly("Should be be used only internally by the Scala compiler")
  def capsOf[CS]: Any = ???

  /** Reach capabilities x* which appear as terms in @retains annotations are encoded
   *  as `caps.reachCapability(x)`. When converted to CaptureRef types in capture sets
   *  they are  represented as `x.type @annotation.internal.reachCapability`.
   */
  extension (x: Any) def reachCapability: Any = x

  /** A trait to allow expressing existential types such as
   *
   *      (x: Exists) => A ->{x} B
   */
  sealed trait Exists extends Capability

  /** This should go into annotations. For now it is here, so that we
   *  can experiment with it quickly between minor releases
   */
  final class untrackedCaptures extends annotation.StaticAnnotation

  object unsafe:

    extension [T](x: T)
      /** A specific cast operation to remove a capture set.
       *  If argument is of type `T^C`, assume it is of type `T` instead.
       *  Calls to this method are treated specially by the capture checker.
       */
      def unsafeAssumePure: T = x

      /** If argument is of type `cs T`, converts to type `box cs T`. This
      *  avoids the error that would be raised when boxing `cap`.
      */
      def unsafeBox: T = x

      /** If argument is of type `box cs T`, converts to type `cs T`. This
       *  avoids the error that would be raised when unboxing `cap`.
       */
      def unsafeUnbox: T = x

    extension [T, U](f: T => U)
      /** If argument is of type `box cs T`, converts to type `cs T`. This
       *  avoids the error that would be raised when unboxing `cap`.
       */
      def unsafeBoxFunArg: T => U = f

  end unsafe
