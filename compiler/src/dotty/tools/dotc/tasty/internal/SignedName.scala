package dotty.tools.dotc.tasty
package internal

import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Names

import scala.tasty.names

object SignedName {

  // TODO make sure all extractors are tested

  def apply(name: Names.TermName): names.SignedName = new Impl(name)

  def unapplySignedName(arg: Impl): Option[names.SignedName.Data] = {
    val name = arg.name
    val NameKinds.SignedName.SignedInfo(sig) = name.info
    Some(TermName(name.underlying), TypeName(sig.resSig), sig.paramsSig.map(TypeName(_)))
  }

  private[tasty] class Impl(val name: Names.TermName) extends names.SignedName {
    override def toString: String = s"SignedName<$name>"
  }

}
