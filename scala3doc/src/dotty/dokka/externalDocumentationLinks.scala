package dotty.dokka

import org.jetbrains.dokka._
import java.net.URL
import scala.util.matching._

case class Scala3docExternalDocumentationLink(
  originRegexes: List[Regex],
  documentationUrl: URL,
  kind: DocumentationKind,
  packageListUrl: Option[URL] = None
):
  def withPackageList(url: URL): Scala3docExternalDocumentationLink = copy(packageListUrl = Some(url))

enum DocumentationKind:
  case Javadoc extends DocumentationKind
  case Scaladoc extends DocumentationKind
  case Scala3doc extends DocumentationKind