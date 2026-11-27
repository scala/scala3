package scala.scalajs.js.internal

import scala.scalajs.js

/** Under -scalajs, this object is part of the implicit scope of `scala.Unit`. */
object UnitOps:
  /** Converts a value of the union type `A | Unit`, which is how `js.UndefOr[A]`
   *  is interpreted under `-scalajs`, to a `js.UndefOrOps[A]` providing
   *  option-like operations such as `map`, `getOrElse` and `foreach`.
   *
   *  @tparam A the type of the value when it is defined
   *  @param x the value to wrap
   *  @return a `js.UndefOrOps[A]` wrapping `x`
   */
  implicit def unitOrOps[A](x: A | Unit): js.UndefOrOps[A] =
    new js.UndefOrOps(x)
