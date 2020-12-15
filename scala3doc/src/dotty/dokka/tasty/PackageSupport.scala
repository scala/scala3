package dotty.dokka
package tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links.PointingToDeclaration
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.model.doc.DocumentationNode
import dotty.dokka._
import dotty.dokka.model.api._

import collection.JavaConverters._

trait PackageSupport:
    self: TastyParser =>
    import qctx.reflect._

    def parsePackage(pck: PackageClause): (String, Member) =
      val name = extractPackageName(pck.pid.show)
      val mExt = MemberExtension.empty.copy(kind = Kind.Package)
      name -> mkMember(pck.symbol, mExt, nameOverride = _ => name)

    def parsePackageObject(pckObj: ClassDef): (String, Member) =
      pckObj.symbol.packageName -> parseClasslike(pckObj).withKind(Kind.Package)

    private def extractPackageName(pidShowNoColor: String): String = {
        val pidSplit = pidShowNoColor.split("\\.")
        pidSplit.mkString("",".","")
    }
