package dotty.tools.dotc.core

import Symbols._

object Annotations {

  abstract class Annotation {
    def matches(cls: Symbol) = ???
    def appliesToModule: Boolean = ???
  }

  abstract class InternalAnnotation extends Annotation {

  }

  case class Alias(sym: Symbol) extends InternalAnnotation {

  }

  case class Child(child: ClassSymbol) extends InternalAnnotation {

  }

}