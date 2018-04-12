package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Names

import scala.tasty.names

object Name {

  def apply(name: Names.Name): names.Name = name match {
    case name: Names.TermName => TermName(name)
    case name: Names.TypeName => TypeName(name)
  }

}
