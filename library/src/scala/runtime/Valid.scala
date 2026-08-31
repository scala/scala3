package scala.runtime

import annotation.experimental

@experimental
case class Valid(elem: Any) extends MaybeCase
