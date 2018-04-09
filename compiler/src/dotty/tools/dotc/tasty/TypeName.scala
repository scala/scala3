package dotty.tools.dotc.tasty

import dotty.tools.dotc.core.Names


object TypeName {

  def apply(name: Names.TypeName): scala.tasty.TypeName = new Impl(name)

  private[tasty] class Impl(val name: Names.TypeName) extends scala.tasty.TypeName {
    override def toString: String = name.toString
  }

}
