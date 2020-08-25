package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, PageNode}
import collection.JavaConverters._

class ScalaEmbeddedResourceAppender extends PageTransformer:
    override def invoke(input: RootPageNode): RootPageNode =
        input.transformContentPagesTree( page =>
            page.modified(
                page.getName,
                page.getContent,
                page.getDri,
                // Remove default CSS and add our own
                (page.getEmbeddedResources.asScala.filterNot(_.endsWith(".css")) :+
                    "styles/scalastyle.css"
                ).asJava,
                page.getChildren
            )
        )
