package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, RendererSpecificResourcePage, RenderingStrategy$Copy, PageNode}
import scala.jdk.CollectionConverters._

class ScalaResourceInstaller extends PageTransformer:
    private def dottyRes(resourceName: String) =
        new RendererSpecificResourcePage(resourceName, java.util.ArrayList(), RenderingStrategy$Copy(s"/dotty_res/$resourceName"))

    override def invoke(input: RootPageNode): RootPageNode =
        val newResources = input.getChildren.asScala ++ Seq("fonts", "images", "styles", "scripts").map(dottyRes)
        input.modified(input.getName, newResources.asJava)
