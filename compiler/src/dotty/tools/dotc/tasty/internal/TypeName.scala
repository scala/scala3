package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Names

import scala.tasty.names

object TypeName {

  def apply(name: Names.TypeName): names.TypeName = Impl(name)

  def unapplyTypeName(arg: names.Name): Option[names.TypeName.Data] = arg match {
    case Impl(name) => Some(TermName(name.toTermName))
    case _ => None
  }

  private[tasty] case class Impl(name: Names.TypeName) extends names.TypeName {
    override def toString: String = name.toString
  }

}
