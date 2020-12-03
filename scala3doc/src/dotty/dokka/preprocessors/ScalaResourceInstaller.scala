package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, RendererSpecificResourcePage, RenderingStrategy$Copy, PageNode, RenderingStrategy$Write}
import scala.jdk.CollectionConverters._
import com.fasterxml.jackson.databind.ObjectMapper
import dotty.dokka.translators.FilterAttributes
import java.nio.file.Paths

class ScalaResourceInstaller(using ctx: DocContext) extends PageTransformer:
  private def dottyRes(resourceName: String) =
    new RendererSpecificResourcePage(resourceName, java.util.ArrayList(), RenderingStrategy$Copy(s"/dotty_res/$resourceName"))

  override def invoke(input: RootPageNode): RootPageNode =
    val dirs = Seq("fonts", "images", "styles", "scripts", "hljs", "favicon.ico")
    val defaultResources = input.getChildren.asScala ++ dirs.map(dottyRes)
    val newResources =
      projectLogo ++ defaultResources ++ Seq(dynamicJsData, scala3docVersionFile)
    input.modified(input.getName, newResources.asJava)

  private def textFile(path: String, content: String) =
    val strategy = RenderingStrategy$Write(content)
    new RendererSpecificResourcePage(path, java.util.ArrayList(), strategy)

  private def dynamicJsData =
    // If data at any point will become more complex we should use a proper mapping
    val data: Map[String, Map[String, String]] =
      Map("filterDefaults" -> FilterAttributes.defaultValues)
    val str = new ObjectMapper().writeValueAsString(data.transform((_, v) => v.asJava).asJava)
    textFile("scripts/data.js", s"var scala3DocData = $str")

  private def scala3docVersionFile = textFile("scala3doc.version", BuildInfo.version)

  private def projectLogo = ctx.args.projectLogo.toSeq.map { path =>
      val fileName = Paths.get(path).getFileName()
      val strategy = new RenderingStrategy$Copy(path)
      new RendererSpecificResourcePage(s"project-logo/$fileName", JList(), strategy)
  }
