package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, RendererSpecificResourcePage, RenderingStrategy$Copy, PageNode}
import collection.JavaConverters
import collection.JavaConverters._

class ScalaEmbeddedResourceAppender extends PageTransformer:
    override def invoke(input: RootPageNode): RootPageNode =
        input.transformContentPagesTree( page =>
            page.modified(
                page.getName,
                page.getContent,
                page.getDri,
                JavaConverters.asJava(page.getEmbeddedResources.asScala.toSeq ++ Seq(
                    "styles/scalastyle.css"
                )),
                page.getChildren
            )
        )
