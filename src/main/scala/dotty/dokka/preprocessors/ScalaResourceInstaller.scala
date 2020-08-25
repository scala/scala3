package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, RendererSpecificResourcePage, RenderingStrategy$Copy, PageNode}
import collection.JavaConverters
import collection.JavaConverters._

class ScalaResourceInstaller extends PageTransformer:
    override def invoke(input: RootPageNode): RootPageNode =
        val logo = new RendererSpecificResourcePage("images", java.util.ArrayList(), RenderingStrategy$Copy("/dotty_res/images"))
        val style = new RendererSpecificResourcePage("styles", java.util.ArrayList(), RenderingStrategy$Copy("/dotty_res/styles"))
        input.modified(input.getName, JavaConverters.asJava(input.getChildren.asScala.toSeq ++ Seq(logo, style)))
