package scala.magic.runtime

import annotation.experimental

@experimental
case class Fail[+E](elem: E)
