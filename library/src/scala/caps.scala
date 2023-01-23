package scala

import annotation.experimental

@experimental object caps:

  /** The universal capture reference */
  val `*`: Any = ()

  /** If argument is of type `cs T`, converts to type `box cs T`. This
   *  avoids the error that would be raised when boxing `*`.
   */
  extension [T](x: T) def unsafeBox: T = x

  /** If argument is of type `box cs T`, converts to type `cs T`. This
   *  avoids the error that would be raised when unboxing `*`.
   */
  extension [T](x: T) def unsafeUnbox: T = x
