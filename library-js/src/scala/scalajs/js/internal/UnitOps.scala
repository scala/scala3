package scala.scalajs.js.internal

import scala.scalajs.js

@deprecated(message = "The implicit conversion was moved to the companion object of `scala.Unit` in Scala.js", since = "3.8.0")
object UnitOps:
  implicit def unitOrOps[A](x: A | Unit): js.UndefOrOps[A] =
    new js.UndefOrOps(x)
