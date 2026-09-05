package scala.runtime

import annotation.experimental

@experimental
case class Fail[+E](elem: E) extends MaybeCase
