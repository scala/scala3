package dotty.tools.dotc.core

import Symbols._

object Annotations {

  abstract class Annotation {
    def matches(cls: Symbol) = ???
    def appliesToModule: Boolean = ???
  }

}