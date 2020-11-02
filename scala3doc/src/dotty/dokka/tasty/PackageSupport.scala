package dotty.dokka.tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.properties._
import org.jetbrains.dokka.model.doc.DocumentationNode
import dotty.dokka._
import dotty.dokka.model.api.CompositeMemberExtension

import collection.JavaConverters._

trait PackageSupport:
    self: TastyParser =>
    import qctx.reflect._

    def parsePackage(pck: PackageClause): DPackage = {
        val name = extractPackageName(pck.pid.show)
        val documentation = pck.symbol.documentation
        DPackage(
          new DRI(name, null, null, PointingToDeclaration.INSTANCE, null),
          Nil.asJava,
          Nil.asJava,
          Nil.asJava,
          Nil.asJava,
          documentation.asJava,
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty()
        )
    }

    def parsePackageObject(pckObj: ClassDef): DPackage =
        parseClasslike(pckObj) match {
          case clazz: DClass =>
            DPackage(
              new DRI(pckObj.symbol.dri.getPackageName, null, null, PointingToDeclaration.INSTANCE, null),
              clazz.getFunctions,
              clazz.getProperties,
              Nil.asJava,
              Nil.asJava,
              pckObj.symbol.documentation.asJava,
              null,
              sourceSet.toSet,
              PropertyContainer.Companion.empty()
                .plus(clazz.get(CompositeMemberExtension))
            )
        }


    private def extractPackageName(pidShowNoColor: String): String = {
        val pidSplit = pidShowNoColor.split("\\.")
        pidSplit.mkString("",".","")
    }
