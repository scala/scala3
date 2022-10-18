package scala

import annotation.experimental

// @experimental , suppress @experimental so we can use in compiler itself
object caps:

  /** If argument is of type `box cs T`, converts to type `cs T`. This
   *  avoids the error that would be raised when unboxing `*`.
   */
  extension [T](x: T) def unsafeUnbox: T = x
