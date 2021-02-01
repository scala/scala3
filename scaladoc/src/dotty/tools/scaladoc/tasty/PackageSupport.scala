package dotty.tools.scaladoc
package tasty

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
