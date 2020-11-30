package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.base.renderers._
import org.jetbrains.dokka.base.resolvers.local._
import org.jetbrains.dokka.base.resolvers.shared._
import org.jetbrains.dokka.base._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.plugability._
import collection.JavaConverters._

class ScalaPackageListCreator(
  context: DokkaContext,
  format: LinkFormat,
  outputFileNames: List[String] = List("package-list"),
  removeModulePrefix: Boolean = true
) extends PageTransformer:
  override def invoke(input: RootPageNode) = {
    input.modified(input.getName, (input.getChildren.asScala ++ packageList(input)).asJava)
  }

  private def processPage(page: PageNode, input: RootPageNode): PageNode = page

  private def packageList(rootPageNode: RootPageNode): List[RendererSpecificPage] = {
    val content = ScalaPackageListService(context, rootPageNode).createPackageList(
      format.getFormatName,
      format.getLinkExtension
    )
    outputFileNames.map( name => {
      RendererSpecificResourcePage(
          s"${rootPageNode.getName}/${name}",
          JList(),
          RenderingStrategy.Write(content)
      )
    }
    )
  }