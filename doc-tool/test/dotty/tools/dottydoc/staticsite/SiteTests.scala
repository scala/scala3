package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class SiteTests extends DottyDocTest {
  private def html(
    str: String,
    params: Map[String, AnyRef] = Map.empty,
    includes: Map[String, String] = Map.empty
  ) = new HtmlPage(str, params, includes)

  @Test def hasCorrectLayoutFiles = {
    val site = new Site(new java.io.File("../doc-tool/resources/"))

    assert(site.root.exists && site.root.isDirectory,
           s"'${site.root.getName}' is not a directory")

    val expectedLayouts = Set("main", "index")
    assert(site.layouts.keys == expectedLayouts,
           s"Incorrect layouts in: ${site.layouts.keys}, expected: $expectedLayouts")
  }

  @Test def renderHelloInMainLayout = {
    val site = new Site(new java.io.File("../doc-tool/resources/"))

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
    val site = new Site(new java.io.File("../doc-tool/resources/"))

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
    val site = new Site(new java.io.File("../doc-tool/resources/"))

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
    val site = new Site(new java.io.File("../doc-tool/resources/"))

    val renderedInclude = site.render(
      html("""{% include "header.html" %}""", includes = site.includes),
      Map.empty
    )

    assertEquals("<h1>Some header</h1>\n", renderedInclude)
  }
}
