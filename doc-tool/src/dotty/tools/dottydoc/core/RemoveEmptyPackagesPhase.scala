package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.Context

import transform.DocMiniPhase
import model._

class RemoveEmptyPackages extends DocMiniPhase {
  override def transformPackage(implicit ctx: Context) = { case p: Package =>
    if (p.members.exists(_.kind != "package")) p
    else NonEntity
  }
}
