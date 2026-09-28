package scala.runtime

import annotation.experimental

@experimental
/** A common superclass for `Valid` and `Fail` */
sealed abstract class MaybeCase

@experimental
/** An internal runtime class used by the `Ok` constructor */
case class Valid(elem: Any) extends MaybeCase

@experimental
/** An internal runtime class used by the `Err` constructor */
case class Fail[+E](elem: E) extends MaybeCase
