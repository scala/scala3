package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Names

import scala.tasty.names

object TypeName {

  def apply(name: Names.TypeName): names.TypeName = new Impl(name)

  private[tasty] class Impl(val name: Names.TypeName) extends names.TypeName {
    override def toString: String = name.toString
  }

}
