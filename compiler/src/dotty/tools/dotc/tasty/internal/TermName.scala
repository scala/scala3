package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.NameKinds

import scala.tasty.names

object TermName {

  // TODO make sure all extractors are tested

  def apply(name: Names.TermName): names.TermName = new Impl(name)

  def unapplySimple(arg: Impl): Option[names.Simple.Data] = arg.name match {
    case name: Names.SimpleName => Some(name.toString)
    case _ => None
  }

  def unapplyQualified(arg: Impl): Option[names.Qualified.Data] = arg.name match {
    case name: Names.DerivedName if name.is(QualifiedName) =>
      Some(TermName(name.underlying), name.lastPart.toString)
    case _ => None
  }

  def unapplyDefaultGetter(arg: Impl): Option[names.DefaultGetter.Data] = arg.name match {
    case name: Names.DerivedName if name.is(DefaultGetterName) =>
      Some(TermName(name.underlying), name.lastPart.toString)
    case _ => None
  }

  def unapplyVariant(arg: Impl): Option[names.Variant.Data] = arg.name match {
    case name: Names.DerivedName if name.is(VariantName) =>
      Some(TermName(name.underlying), name.info.asInstanceOf[NumberedInfo].num == 1)
    case _ => None
  }

  def unapplySuperAccessor(arg: Impl): Option[names.SuperAccessor.Data] = arg.name match {
    case name: Names.DerivedName if name.is(SuperAccessorName) => Some(TermName(name.underlying))
    case _ => None
  }

  def unapplyProtectedAccessor(arg: Impl): Option[names.ProtectedAccessor.Data] = arg.name match {
    case name: Names.DerivedName if name.is(ProtectedAccessorName) => Some(TermName(name.underlying))
    case _ => None
  }

  def unapplyProtectedSetter(arg: Impl): Option[names.ProtectedSetter.Data] = arg.name match {
    case name: Names.DerivedName if name.is(ProtectedSetterName) => Some(TermName(name.underlying))
    case _ => None
  }

  def unapplyObjectClass(arg: Impl): Option[names.ObjectClass.Data] = arg.name match {
    case name: Names.DerivedName if name.is(NameKinds.ModuleClassName) => Some(TermName(name.underlying))
    case _ => None
  }

  private[tasty] class Impl(val name: Names.TermName) extends names.TermName {
    override def toString: String = s"TermName<$name>"
  }

}
