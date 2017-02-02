package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

class PageTests extends DottyDocTest with SourceFileOps {
  import scala.collection.JavaConverters._

  @Test def mdHas1Key = {
    val page = markdownPage(
      """|---
         |key:
         |---
         |
         |great""".stripMargin
    )

    assert(
      page.yaml == Map("key" -> List.empty.asJava),
      s"""incorrect yaml, expected "key:" without key in: ${page.yaml}"""
    )

    assertEquals("<p>great</p>\n", page.html.get)
  }

  @Test def yamlPreservesLiquidTags = {
    val page1 = markdownPage(
      """|---
         |key:
         |---
         |
         |{{ content }}""".stripMargin,
       params = Map("content" -> "Hello, world!")
    )

    assert(
      page1.yaml == Map("key" -> List.empty.asJava),
      s"""incorrect yaml, expected "key:" without key in: ${page1.yaml}"""
    )

    assertEquals("<p>Hello, world!</p>\n", page1.html.get)

    val page2 = markdownPage(
      """|{{ content }}""".stripMargin,
      params = Map("content" -> "hello")
    )
    assert(
      page2.yaml == Map(),
      s"""incorrect yaml, expected "key:" without key in: ${page2.yaml}"""
    )
    assertEquals("<p>hello</p>\n", page2.html.get)

    val page3 = markdownPage(
      """|{% if product.title == "Awesome Shoes" %}
         |These shoes are awesome!
         |{% endif %}""".stripMargin,
      params = Map("product" -> Map("title" -> "Awesome Shoes").asJava)
    )

    assertEquals(
      "<p>These shoes are awesome!</p>\n",
      page3.html.get
    )
  }

  @Test def simpleHtmlPage = {
    val p1 = htmlPage("""<h1>{{ "hello, world!" }}</h1>""")
    assert(p1.yaml == Map(), "non-empty yaml found")
    assertEquals("<h1>hello, world!</h1>", p1.html.get)
  }

  @Test def htmlPageHasNoYaml = {
    val page = htmlPage(
      """|---
         |layout: main
         |---
         |
         |Hello, world!""".stripMargin
    )

    assert(!page.html.get.contains("---\nlayout: main\n---"),
           s"page still contains yaml:\n${page.html.get}")
  }

  @Test def illegalYamlFrontMatter = try {
    val page = htmlPage(
      """|---
         |layout: main
         |
         |
         |Hello, world!""".stripMargin
    )

    page.html.get
    fail("illegal front matter didn't throw exception")
  } catch {
    case IllegalFrontMatter(x) => // success!
    case t: Throwable => throw t
  }
}
