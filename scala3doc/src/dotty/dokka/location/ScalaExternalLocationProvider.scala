package dotty.dokka

import org.jetbrains.dokka.base.resolvers.local._
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.base.resolvers.external._
import org.jetbrains.dokka.base.resolvers.shared._
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.pages.RootPageNode
import org.jetbrains.dokka.plugability._
import collection.JavaConverters._
import java.util.{Set => JSet}


class ScalaExternalLocationProvider(
  externalDocumentation: ExternalDocumentation,
  extension: String,
  kind: DocumentationKind
)(using ctx: DokkaContext) extends DefaultExternalLocationProvider(externalDocumentation, extension, ctx):
  override def resolve(dri: DRI): String =
    Option(externalDocumentation.getPackageList).map(_.getLocations.asScala.toMap).flatMap(_.get(dri.toString))
      .fold(constructPath(dri))( l => {
        this.getDocURL + l
      }
    )

  private val originRegex = raw"\[origin:(.*)\]".r

  override def constructPath(dri: DRI): String = kind match {
    case DocumentationKind.Javadoc => constructPathForJavadoc(dri)
    case DocumentationKind.Scaladoc => constructPathForScaladoc(dri)
    case DocumentationKind.Scala3doc => constructPathForScala3doc(dri)
  }

  private def constructPathForJavadoc(dri: DRI): String = {
    val packagePrefix = dri.getPackageName.replace(".","/")
    val origin = originRegex.findFirstIn(dri.getExtra)
    val className = origin match {
      case Some(path) =>
        path.split("/").last.stripSuffix(".class")
      case None => dri.getClassNames
    }
    getDocURL + packagePrefix + "/" + className + extension
  }

  private def constructPathForScaladoc(dri: DRI): String = {
    val packagePrefix = dri.getPackageName.replace(".","/")
    val className = dri.getClassNames
    getDocURL + packagePrefix + "/" + className + extension
  }

  private def constructPathForScala3doc(dri: DRI): String = super.constructPath(dri)
