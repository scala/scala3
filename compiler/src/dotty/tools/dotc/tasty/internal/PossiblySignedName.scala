package dotty.tools.dotc.tasty.internal

import dotty.tools.dotc.core.{NameKinds, Names}

import scala.tasty.names

object PossiblySignedName {

  def apply(name: Names.TermName): names.PossiblySignedName =
    if (name.is(NameKinds.SignedName)) SignedName(name)
    else TermName(name)

}
