package scala.magic.runtime

import annotation.experimental

@experimental
class Invalid[+E](val elem: E)
