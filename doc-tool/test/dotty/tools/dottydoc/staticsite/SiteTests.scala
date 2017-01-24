package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class SiteTests extends DottyDocTest {
  import scala.collection.JavaConverters._
  val site = new Site(new java.io.File("../doc-tool/resources/"), "test-site", Map.empty)

  private def html(
    str: String,
    params: Map[String, AnyRef] = Map("docs" -> List.empty.asJava),
    includes: Map[String, String] = Map.empty
  ) = new HtmlPage(str, params, includes)

  @Test def hasCorrectLayoutFiles = {
    assert(site.root.exists && site.root.isDirectory,
           s"'${site.root.getName}' is not a directory")

    val expectedLayouts = Set("main", "index", "sidebar", "blog-page", "doc-page", "api-page")
    assert(site.layouts.keys == expectedLayouts,
           s"Incorrect layouts in: ${site.layouts.keys}, expected: $expectedLayouts")
  }

  @Test def renderHelloInMainLayout = {
    val renderedPage = site.render(html(
      """|---
         |layout: main
         |---
         |
         |Hello, world!""".stripMargin
    ), Map.empty)

    assert(
      renderedPage.contains("Hello, world!") &&
      !renderedPage.contains("---\nlayout: main\n---\n") &&
      renderedPage.contains("<!DOCTYPE html>"),
      "html page did not render properly"
    )
  }

  @Test def renderMultipleTemplates = {
    val renderedPage = site.render(html(
      """|---
         |layout: index
         |---
         |Hello, world!""".stripMargin
    ), Map.empty)

    assert(
      renderedPage.contains("<h1>Hello, world!</h1>") &&
      !renderedPage.contains("---\nlayout: main\n---\n") &&
      !renderedPage.contains("---\nlayout: index\n---\n") &&
      renderedPage.contains("<!DOCTYPE html>"),
      "html page did not render properly"
    )
  }

  @Test def preservesPageYaml = {
    val renderedPage = site.render(html(
      """|---
         |title: Hello, world
         |layout: index
         |---
         |Hello, world!""".stripMargin
    ), Map.empty)

    assert(
      renderedPage.contains("<h1>Hello, world!</h1>") &&
      !renderedPage.contains("---\nlayout: main\n---\n") &&
      !renderedPage.contains("---\nlayout: index\n---\n") &&
      renderedPage.contains("<title>Hello, world</title>") &&
      renderedPage.contains("<!DOCTYPE html>"),
      "html page did not render properly:\n" + renderedPage
    )
  }

  @Test def include = {
    val renderedInclude = site.render(
      html("""{% include "header.html" %}""", includes = site.includes),
      Map.empty
    )

    assertEquals("<h1>Some header</h1>\n", renderedInclude)
  }

  @Test def siteStructure = {
    val assets = site.staticAssets.map(site.stripRoot).toSet
    val compd  = site.compilableFiles.map(site.stripRoot).toSet

    val expectedAssets = Set(
      "css/api-page.css",
      "css/dottydoc.css",
      "css/color-brewer.css",
      "js/highlight.pack.js"
    )
    val expectedCompd = Set(
      // Directories starting in `_` are not included in compilable files
      "index.md"
    )

    assert(expectedAssets == assets,
           s"assets incorrect, found: $assets - expected $expectedAssets")
    assert(expectedCompd == compd,
           s"compilable files incorrect, found: $compd - expected $expectedCompd")
  }
}
