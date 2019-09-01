package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class SiteTests extends DottyDocTest with SourceFileOps with CheckFromSource {
  @Test def hasCorrectLayoutFiles = {
    assert(site.root.exists && site.root.isDirectory,
           s"'${site.root.getName}' is not a directory")

    val expectedLayouts = Set("base", "main", "index", "blog-page", "doc-page", "api-page", "search")
    assert(site.layouts.keys == expectedLayouts,
           s"Incorrect layouts in: ${site.layouts.keys}, expected: $expectedLayouts")
  }

  @Test def renderHelloInMainLayout = {
    val renderedPage = site.render(htmlPage(
      """|---
         |layout: main
         |---
         |
         |Hello, world!""".stripMargin
    ), Map.empty).get

    assert(
      renderedPage.contains("Hello, world!") &&
      !renderedPage.contains("---\nlayout: main\n---\n") &&
      renderedPage.contains("<!DOCTYPE html>"),
      "html page did not render properly"
    )
  }

  @Test def renderMultipleTemplates = {
    val renderedPage = site.render(htmlPage(
      """|---
         |layout: index
         |---
         |Hello, world!""".stripMargin
    ), Map.empty).get

    assert(
      renderedPage.contains("<h1>Hello, world!</h1>") &&
      !renderedPage.contains("---\nlayout: main\n---\n") &&
      !renderedPage.contains("---\nlayout: index\n---\n") &&
      renderedPage.contains("<!DOCTYPE html>"),
      "html page did not render properly"
    )
  }

  @Test def preservesPageYaml = {
    val renderedPage = site.render(htmlPage(
      """|---
         |title: Hello, world
         |layout: index
         |---
         |Hello, world!""".stripMargin
    ), Map.empty).get

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
      htmlPage("""{% include "header.html" %}""", includes = site.includes),
      Map.empty
    ).get

    assertEquals("<h1>Some header</h1>\n", renderedInclude)
  }

  @Test def siteStructure = {
    val assets = site.staticAssets.map(site.stripRoot(_).replace('\\','/')).toSet
    val compd  = site.compilableFiles.map(site.stripRoot(_).replace('\\','/')).toSet

    val expectedAssets = Set(
      "css/bootstrap.min.css",
      "css/color-brewer.css",
      "css/dottydoc.css",
      "css/search.css",
      "css/sidebar.css",
      "css/toolbar.css",
      "images/dotty-logo-white.svg",
      "images/dotty-logo.svg",
      "images/scala-logo-white.svg",
      "images/scala-logo.svg",
      "js/api-search.js",
      "js/bootstrap.min.js",
      "js/dottydoc.js",
      "js/highlight.pack.js",
      "js/jquery.min.js",
      "js/sidebar.js",
      "js/toolbar.js"
    )
    val expectedCompd = Set(
      // Directories starting in `_` are not included in compilable files
      "index.md"
    )

    def printSet(xs: Set[String]): String =
      xs.toList.sorted.mkString("\n{\n  ", ",\n  ", "\n}")

    assert(expectedAssets == assets,
           s"assets incorrect, found: ${ printSet(assets) } - expected ${ printSet(expectedAssets) }")
    assert(expectedCompd == compd,
           s"compilable files incorrect, found: ${ printSet(compd) } - expected ${ printSet(expectedCompd) }")
  }
}
