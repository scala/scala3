package scala.scalajs.js.internal

import scala.scalajs.js

/** Under -scalajs, this object is part of the implicit scope of `scala.Unit`. */
object UnitOps:
  implicit def unitOrOps[A](x: A | Unit): js.UndefOrOps[A] =
    new js.UndefOrOps(x)
