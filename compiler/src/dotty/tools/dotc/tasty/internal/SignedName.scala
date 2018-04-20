package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Names

import scala.tasty.names

object SignedName {

  // TODO make sure all extractors are tested

  def apply(name: Names.TermName): names.SignedName = Impl(name)

  def unapplySignedName(arg: names.PossiblySignedName): Option[names.SignedName.Data] = arg match {
    case Impl(name: Names.DerivedName) if name.is(NameKinds.SignedName) =>
      val NameKinds.SignedName.SignedInfo(sig) = name.info
      Some(TermName(name.underlying), TypeName(sig.resSig), sig.paramsSig.map(TypeName(_)))
    case _ => None
  }

  private[tasty] case class Impl(name: Names.TermName) extends names.SignedName {
    override def toString: String = {
      import Toolbox.extractor
      val names.SignedName(underlying, resSig, paramsSigs) = this
      s"SignedName($underlying, $resSig, $paramsSigs)"
    }
  }

}
