package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class PageTests extends DottyDocTest {
  import scala.collection.JavaConverters._

  @Test def mdHas1Key = {
    val page = new MarkdownPage(
      """|---
         |key:
         |---
         |
         |great""".stripMargin,
      Map.empty,
      Map.empty
    )

    assert(
      page.yaml == Map("key" -> ""),
      s"""incorrect yaml, expected "key:" without key in: ${page.yaml}"""
    )

    assertEquals("<p>great</p>\n", page.html)
  }

  @Test def yamlPreservesLiquidTags = {
    val page1 = new MarkdownPage(
      """|---
         |key:
         |---
         |
         |{{ content }}""".stripMargin,
       Map("content" -> "Hello, world!"),
       Map.empty
    )

    assert(
      page1.yaml == Map("key" -> ""),
      s"""incorrect yaml, expected "key:" without key in: ${page1.yaml}"""
    )

    assertEquals("<p>Hello, world!</p>\n", page1.html)

    val page2 = new MarkdownPage(
      """|{{ content }}""".stripMargin,
      Map("content" -> "hello"),
      Map.empty
    )
    assert(
      page2.yaml == Map(),
      s"""incorrect yaml, expected "key:" without key in: ${page2.yaml}"""
    )
    assertEquals("<p>hello</p>\n", page2.html)

    val page3 = new MarkdownPage(
      """|{% if product.title == "Awesome Shoes" %}
         |These shoes are awesome!
         |{% endif %}""".stripMargin,
      Map("product" -> Map("title" -> "Awesome Shoes").asJava),
      Map.empty
    )

    assertEquals(
      "<p>These shoes are awesome!</p>\n",
      page3.html
    )
  }

  @Test def simpleHtmlPage = {
    val p1 = new HtmlPage("""<h1>{{ "hello, world!" }}</h1>""", Map.empty, Map.empty)
    assert(p1.yaml == Map(), "non-empty yaml found")
    assertEquals("<h1>hello, world!</h1>", p1.html)
  }

  @Test def htmlPageHasNoYaml = {
    val page = new HtmlPage(
      """|---
         |layout: main
         |---
         |
         |Hello, world!""".stripMargin,
      Map.empty,
      Map.empty
    )

    assert(!page.html.contains("---\nlayout: main\n---"),
           s"page still contains yaml:\n${page.html}")
  }

  @Test def illegalYamlFrontMatter = try {
    val page = new HtmlPage(
      """|---
         |layout: main
         |
         |
         |Hello, world!""".stripMargin,
      Map.empty,
      Map.empty
    )

    page.html
    fail("illegal front matter didn't throw exception")
  } catch {
    case IllegalFrontMatter(x) => // success!
    case t: Throwable => throw t
  }
}
