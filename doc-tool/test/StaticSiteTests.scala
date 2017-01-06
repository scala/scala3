package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import staticsite.{ Site, HtmlPage }

class StaticSiteTests extends DottyDocTest {
  @Test def hasCorrectLayoutFiles = {
    val site = new Site(new java.io.File("../doc-tool/resources/"))

    assert(site.root.exists && site.root.isDirectory,
           s"'${site.root.getName}' is not a directory")

    val expectedLayouts = Set("main")
    assert(site.layouts.keys == expectedLayouts,
           s"Incorrect layouts in: ${site.layouts.keys}, expected: $expectedLayouts")
  }

  @Test def renderHelloInMainLayout = {
    val site = new Site(new java.io.File("../doc-tool/resources/"))

    val renderedPage = site.render(new HtmlPage(
      """|---
         |layout: main
         |---
         |
         |Hello, world!""".stripMargin,
      Map.empty
    ), Map.empty)

    assert(
      renderedPage.contains("Hello, world!") &&
      !renderedPage.contains("---\nlayout: main\n---\n") &&
      renderedPage.contains("<!DOCTYPE html>"),
      "html page did not render properly"
    )
  }
}
