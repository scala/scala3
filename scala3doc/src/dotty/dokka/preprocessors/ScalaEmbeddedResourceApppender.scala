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
        // Remove default CSS and navigation loader and add our own  versions
        (page.getEmbeddedResources.asScala
          .filterNot(_ == "scripts/navigation-loader.js")
          .filterNot(_.endsWith(".css")) ++ Seq(
          "styles/nord-light.css",
          "styles/scalastyle.css",
          "styles/dotty-icons.css",
          "styles/diagram.css",
          "styles/filter-bar.css",
          "styles/search-bar.css",
          "https://code.jquery.com/jquery-3.5.1.min.js",
          "https://d3js.org/d3.v6.min.js",
          "https://cdn.jsdelivr.net/npm/graphlib-dot@0.6.2/dist/graphlib-dot.min.js",
          "https://cdnjs.cloudflare.com/ajax/libs/dagre-d3/0.6.1/dagre-d3.min.js",
          "scripts/diagram.js",
          "styles/filter-bar.css",
          "hljs/highlight.pack.js",
          "scripts/hljs-scala3.js",
          "scripts/ux.js",
          "scripts/common/component.js",
          "scripts/common/utils.js",
          "scripts/components/FilterBar.js",
          "scripts/components/DocumentableList.js",
          "scripts/components/Input.js",
          "scripts/components/FilterGroup.js",
          "scripts/components/Filter.js",
          "scripts/data.js",
          "scripts/fast-navigation-loader.js"
        )).asJava,
        page.getChildren
      )
    )
}
