package dotty.tools
package dottydoc
package core

import dotty.tools.dotc.core.Symbols.Symbol
import dotc.core.Contexts.Context
import dotc.ast.tpd

import model._
import model.internal._
import util.syntax._
import transform.DocMiniPhase

class PackageObjectsPhase extends DocMiniPhase {

  override def transformPackage(implicit ctx: Context) = { case pkg: PackageImpl =>
    pkg
      .members
      .collect { case o: Object if o.symbol.isPackageObject => o }
      .headOption
      .map { obj =>
        pkg.copy(
          members = obj.members ++ pkg.members,
          superTypes = obj.superTypes,
          comment = obj.comment
        )
      }
      .getOrElse(pkg)
  }

  override def transformObject(implicit ctx: Context) = { case obj: Object =>
    if (obj.symbol.isPackageObject) NonEntity
    else obj
  }
}
