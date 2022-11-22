package scala

import annotation.experimental

@experimental object caps:

  /** The universal capture reference */
  val `*`: Any = ()

  object unsafe:

    /** If argument is of type `cs T`, converts to type `box cs T`. This
     *  avoids the error that would be raised when boxing `*`.
     */
    extension [T](x: T) def unsafeBox: T = x

    /** If argument is of type `box cs T`, converts to type `cs T`. This
     *  avoids the error that would be raised when unboxing `*`.
     */
    extension [T](x: T) def unsafeUnbox: T = x

    /** If argument is of type `box cs T`, converts to type `cs T`. This
     *  avoids the error that would be raised when unboxing `*`.
     */
    extension [T, U](f: T => U) def unsafeBoxFunArg: T => U = f
  end unsafe

  /** Mixing in this trait forces a trait or class to be pure, i.e.
   *  have no capabilities retained in its self type.
   */
  trait Pure:
    this: Pure =>
