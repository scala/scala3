package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.NameKinds._
import dotty.tools.dotc.core.NameKinds

import scala.tasty.names

object TermName {

  // TODO make sure all extractors are tested

  def apply(name: Names.TermName): names.TermName = Impl(name)

  def unapplySimple(arg: names.Name): Option[names.Simple.Data] = arg match {
    case Impl(name: Names.SimpleName) => Some(name.toString)
    case _ => None
  }

  def unapplyQualified(arg: names.Name): Option[names.Qualified.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(QualifiedName) =>
      Some(TermName(name.underlying), name.lastPart.toString)
    case _ => None
  }

  def unapplyDefaultGetter(arg: names.Name): Option[names.DefaultGetter.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(DefaultGetterName) =>
      Some(TermName(name.underlying), name.lastPart.toString)
    case _ => None
  }

  def unapplyVariant(arg: names.Name): Option[names.Variant.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(VariantName) =>
      Some(TermName(name.underlying), name.info.asInstanceOf[NumberedInfo].num == 1)
    case _ => None
  }

  def unapplySuperAccessor(arg: names.Name): Option[names.SuperAccessor.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(SuperAccessorName) => Some(TermName(name.underlying))
    case _ => None
  }

  def unapplyProtectedAccessor(arg: names.Name): Option[names.ProtectedAccessor.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(ProtectedAccessorName) => Some(TermName(name.underlying))
    case _ => None
  }

  def unapplyProtectedSetter(arg: names.Name): Option[names.ProtectedSetter.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(ProtectedSetterName) => Some(TermName(name.underlying))
    case _ => None
  }

  def unapplyObjectClass(arg: names.Name): Option[names.ObjectClass.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(NameKinds.ModuleClassName) => Some(TermName(name.underlying))
    case _ => None
  }

  private[tasty] case class Impl(name: Names.TermName) extends names.TermName {
    override def toString: String = name.toString
  }

}
