package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.{Context, ctx}

import transform.DocMiniPhase
import model._

class RemoveEmptyPackages extends DocMiniPhase {
  override def transformPackage(using Context) = { case p: Package =>
    if (p.members.exists(_.kind != "package")) p :: Nil
    else Nil
  }
}
