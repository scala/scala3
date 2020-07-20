package dotty.tools
package dottydoc
package core

import dotc.core.Contexts.{Context, ctx}
import model._
import model.internal._
import transform.DocMiniPhase

class PackageObjectsPhase extends DocMiniPhase {

  override def transformPackage(using Context) = { case pkg: PackageImpl =>
    pkg
      .members
      .collectFirst { case o: Object if o.symbol.isPackageObject => o }
      .map { obj =>
        pkg.copy(
          members = obj.members ++ pkg.members,
          superTypes = obj.superTypes,
          comment = obj.comment
        )
      }
      .getOrElse(pkg) :: Nil
  }

  override def transformObject(using Context) = { case obj: Object =>
    if (obj.symbol.isPackageObject) Nil
    else obj :: Nil
  }
}
