package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Names

import scala.tasty.names

object TypeName {

  def apply(name: Names.TypeName): names.TypeName = new Impl(name)

  def unapplyTypeName(arg: Impl): Option[names.TypeName.Data] = {
    Some(TermName(arg.name.toTermName))
  }

  private[tasty] class Impl(val name: Names.TypeName) extends names.TypeName {
    override def toString: String = s"TypeName<$name>"
  }

}
