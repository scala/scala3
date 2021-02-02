package dotty.dokka
package tasty

import dotty.dokka._
import dotty.dokka.model.api._

import collection.JavaConverters._

trait PackageSupport:
    self: TastyParser =>
    import qctx.reflect._

    def parsePackage(pck: PackageClause): (String, Member) =
      val name = extractPackageName(pck.pid.show)
      (name, Member(name, pck.symbol.dri, Kind.Package))

    def parsePackageObject(pckObj: ClassDef): (String, Member) =
      pckObj.symbol.packageName -> parseClasslike(pckObj).withKind(Kind.Package)

    private def extractPackageName(pidShowNoColor: String): String = {
        val pidSplit = pidShowNoColor.split("\\.")
        pidSplit.mkString("",".","")
    }
