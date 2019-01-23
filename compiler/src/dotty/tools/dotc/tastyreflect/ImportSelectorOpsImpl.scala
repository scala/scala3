/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.core.StdNames.nme

trait ImportSelectorOpsImpl extends scala.tasty.reflect.ImportSelectorOps with CoreImpl {

  object SimpleSelector extends SimpleSelectorModule {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[Id] = x match {
      case x: untpd.Ident => Some(x)
      case _ => None
    }
  }

  object RenameSelector extends RenameSelectorModule {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[(Id, Id)] = x match {
      case Trees.Thicket((id1: untpd.Ident) :: (id2: untpd.Ident) :: Nil) if id2.name != nme.WILDCARD => Some(id1, id2)
      case _ => None
    }
  }

  object OmitSelector extends OmitSelectorModule {
    def unapply(x: ImportSelector)(implicit ctx: Context): Option[Id] = x match {
      case Trees.Thicket((id: untpd.Ident) :: Trees.Ident(nme.WILDCARD) :: Nil) => Some(id)
      case _ => None
    }
  }

}
