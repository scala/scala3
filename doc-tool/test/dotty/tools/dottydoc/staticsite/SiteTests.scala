package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class SiteTests extends DottyDocTest {
  val site = new Site(new java.io.File("../doc-tool/resources/"))

  private def html(
    str: String,
    params: Map[String, AnyRef] = Map.empty,
    includes: Map[String, String] = Map.empty
  ) = new HtmlPage(str, params, includes)

  @Test def hasCorrectLayoutFiles = {
    assert(site.root.exists && site.root.isDirectory,
           s"'${site.root.getName}' is not a directory")

    val expectedLayouts = Set("main", "index")
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
      "html page did not render properly"
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
      "css/dottydoc.css"
    )
    val expectedCompd = Set(
      "index.md"
      // Directories starting in `_` are not included in compilable files
      //"_includes/header.html",
      //"_layouts/index.html",
      //"_layouts/main.html"
    )

    assert(expectedAssets == assets,
           s"assets incorrect, found: $assets - expected $expectedAssets")
    assert(expectedCompd == compd,
           s"compilable files incorrect, found: $compd - expected $expectedCompd")
  }
}
