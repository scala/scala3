package dotty.dokka

import org.jetbrains.dokka.transformers.pages.{PageTransformer}
import org.jetbrains.dokka.pages.{RootPageNode, PageNode}
import scala.jdk.CollectionConverters._

class ScalaEmbeddedResourceAppender extends PageTransformer {
  override def invoke(input: RootPageNode): RootPageNode =
    input.transformContentPagesTree(page =>
      page.modified(
        page.getName,
        page.getContent,
        page.getDri,
        // Remove default CSS and add our own
        (page.getEmbeddedResources.asScala
          .filterNot(_.endsWith(".css")) ++ Seq(
          "styles/nord-light.css",
          "styles/scalastyle.css",
          "styles/dotty-icons.css",
          "hljs/highlight.pack.js",
          "scripts/hljs-scala3.js",
          "scripts/ux.js"
        )).asJava,
        page.getChildren
      )
    )
}
