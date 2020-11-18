package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, RendererSpecificResourcePage, RenderingStrategy$Copy, PageNode, RenderingStrategy$Write}
import scala.jdk.CollectionConverters._
import com.fasterxml.jackson.databind.ObjectMapper
import dotty.dokka.translators.FilterAttributes
import java.nio.file.Paths

class ScalaResourceInstaller(args: Args) extends PageTransformer:
  private def dottyRes(resourceName: String) =
    new RendererSpecificResourcePage(resourceName, java.util.ArrayList(), RenderingStrategy$Copy(s"/dotty_res/$resourceName"))

  override def invoke(input: RootPageNode): RootPageNode =
    val defaultResources = input.getChildren.asScala ++ Seq("fonts", "images", "styles", "scripts", "hljs", "favicon.ico").map(dottyRes)
    val newResources = projectLogo ++ defaultResources ++ Seq(dynamicJsData)
    input.modified(input.getName, newResources.asJava)

  private def dynamicJsData =
    // If data at any point will become more complex we should use a proper
    val data: Map[String, Map[String, String]] = Map("filterDefaults" -> FilterAttributes.defaultValues)
    val str = new ObjectMapper().writeValueAsString(data.transform((_, v) => v.asJava).asJava)

    new RendererSpecificResourcePage("scripts/data.js", java.util.ArrayList(), RenderingStrategy$Write(s"var scala3DocData = $str"))

  private def projectLogo = args.projectLogo.toSeq.map { path =>
      val fileName = Paths.get(path).getFileName()
      val strategy = new RenderingStrategy$Copy(path)
      new RendererSpecificResourcePage(s"project-logo/$fileName", JList(), strategy)
  }
