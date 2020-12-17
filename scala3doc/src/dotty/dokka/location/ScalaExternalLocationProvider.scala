package dotty.dokka

import org.jetbrains.dokka.base.resolvers.local._
import org.jetbrains.dokka.base.DokkaBase
import org.jetbrains.dokka.base.resolvers.external._
import org.jetbrains.dokka.base.resolvers.shared._
import org.jetbrains.dokka.base.resolvers.anchors._
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.DisplaySourceSet
import org.jetbrains.dokka.pages.RootPageNode
import dotty.dokka.model.api._
import org.jetbrains.dokka.plugability._
import collection.JavaConverters._
import java.util.{Set => JSet}


class ScalaExternalLocationProvider(
  externalDocumentation: ExternalDocumentation,
  extension: String,
  kind: DocumentationKind
) extends ExternalLocationProvider:
  def docURL = externalDocumentation.getDocumentationURL.toString.stripSuffix("/") + "/"
  override def resolve(dri: DRI): String =
    Option(externalDocumentation.getPackageList).map(_.getLocations.asScala.toMap).flatMap(_.get(dri.toString))
      .fold(constructPath(dri))( l => {
        this.docURL + l
      }
    )

  private val originRegex = raw"\[origin:(.*)\]".r

  def constructPath(dri: DRI): String = kind match {
    case DocumentationKind.Javadoc => constructPathForJavadoc(dri)
    case DocumentationKind.Scaladoc => constructPathForScaladoc(dri)
    case DocumentationKind.Scala3doc => constructPathForScala3doc(dri)
  }

  private def constructPathForJavadoc(dri: DRI): String = {
    val location = "\\$+".r.replaceAllIn(dri.location.replace(".","/"), _ => ".")
    val origin = originRegex.findFirstIn(dri.extra)
    val anchor = dri.anchor
    docURL + location + extension + anchor.fold("")(a => s"#$a")
  }

  private def constructPathForScaladoc(dri: DRI): String = {
    val location = dri.location.replace(".","/")
    val anchor = dri.anchor
    docURL + location + extension + anchor.fold("")(a => s"#$a")
  }

  private def constructPathForScala3doc(dri: DRI): String = {
    val location = dri.location.replace(".","/")
    val anchor = dri.anchor
    docURL + location + anchor.fold(extension)(a => s"/$a$extension")
  }
