package dotty.tools
package dottydoc
package util

import dotc.core.Contexts.{ Context, DocBase }
import model.Package

object syntax {
  implicit class RichDocContext(val ctx: Context) extends AnyVal {
    def docbase: DocBase = ctx.getDocbase getOrElse {
      throw new IllegalStateException("DocBase must be set before running dottydoc phases")
    }
  }

  implicit class RichDocBase(val db: DocBase) {
    def packages: Map[String, Package] = db.packagesAs[Package].toMap

    def packagesMutable: collection.mutable.Map[String, Package] =
      db.packagesAs[Package]
  }
}
