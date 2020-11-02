package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, RendererSpecificResourcePage, RenderingStrategy$Copy, PageNode, RenderingStrategy$Write}
import scala.jdk.CollectionConverters._
import com.fasterxml.jackson.databind.ObjectMapper
import dotty.dokka.translators.FilterAttributes

class ScalaResourceInstaller extends PageTransformer:
    private def dottyRes(resourceName: String) =
        new RendererSpecificResourcePage(resourceName, java.util.ArrayList(), RenderingStrategy$Copy(s"/dotty_res/$resourceName"))

    override def invoke(input: RootPageNode): RootPageNode =
        val newResources = input.getChildren.asScala ++ Seq("fonts", "images", "styles", "scripts", "hljs").map(dottyRes) ++ Seq(dynamicJsData)
        input.modified(input.getName, newResources.asJava)

    private def dynamicJsData =
        // If data at any point will become more complex we should use a proper
        val data: Map[String, Map[String, String]] = Map("filterDefaults" -> FilterAttributes.defaultValues)
        val str = new ObjectMapper().writeValueAsString(data.transform((_, v) => v.asJava).asJava)

        new RendererSpecificResourcePage("scripts/data.js", java.util.ArrayList(), RenderingStrategy$Write(s"var scala3DocData = $str"))
