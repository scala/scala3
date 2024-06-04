package scala

import annotation.experimental

@experimental object caps:

  trait Capability extends Any

  /** The universal capture reference */
  val cap: Capability = new Capability() {}

  /** The universal capture reference (deprecated) */
  @deprecated("Use `cap` instead")
  val `*`: Capability = cap

  @deprecated("Use `Capability` instead")
  type Cap = Capability

  /** Reach capabilities x* which appear as terms in @retains annotations are encoded
   *  as `caps.reachCapability(x)`. When converted to CaptureRef types in capture sets
   *  they are  represented as `x.type @annotation.internal.reachCapability`.
   */
  extension (x: Any) def reachCapability: Any = x

  object unsafe:

    extension [T](x: T)
      /** A specific cast operation to remove a capture set.
       *  If argument is of type `T^C`, assume it is of type `T` instead.
       *  Calls to this method are treated specially by the capture checker.
       */
      def unsafeAssumePure: T = x

      /** If argument is of type `cs T`, converts to type `box cs T`. This
      *  avoids the error that would be raised when boxing `*`.
      */
      @deprecated(since = "3.3")
      def unsafeBox: T = x

      /** If argument is of type `box cs T`, converts to type `cs T`. This
       *  avoids the error that would be raised when unboxing `*`.
       */
      @deprecated(since = "3.3")
      def unsafeUnbox: T = x

    extension [T, U](f: T => U)
      /** If argument is of type `box cs T`, converts to type `cs T`. This
       *  avoids the error that would be raised when unboxing `*`.
       */
      @deprecated(since = "3.3")
      def unsafeBoxFunArg: T => U = f

  end unsafe
