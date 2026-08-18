package scala.magic.runtime

import annotation.experimental

@experimental
class Fail[+E](val elem: E):
  override def toString = s"Fail($elem)"
