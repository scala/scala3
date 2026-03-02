package dotty.tools.scaladoc
package tasty

import scala.jdk.CollectionConverters._

import SymOps._

import dotty.tools.scaladoc.cc.CCImport

trait PackageSupport:
    self: TastyParser =>
    import qctx.reflect._

    private given qctx.type = qctx

    def parsePackage(pck: PackageClause): (String, Member) =
      val name = pck.symbol.fullName
      ccFlag = false
      pck.stats.foreach {
          case CCImport() => ccFlag = true
          case _ =>
      }
      (name, Member(name, "", pck.symbol.dri, Kind.Package))

    def parsePackageObject(pckObj: ClassDef): (String, Member) =
      pckObj.symbol.packageName -> parseClasslike(pckObj).withKind(Kind.Package)
