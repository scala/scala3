package dotty.tools
package dottydoc
package staticsite

import org.junit.Test
import org.junit.Assert._

import model.Package

class PageTests extends DottyDocTest with SourceFileOps {
  import scala.collection.JavaConverters._

  private def markdownPage(
    sourceCode: String,
    path: String = "test-page",
    params: Map[String, AnyRef] = Map.empty,
    includes: Map[String, Include] = Map.empty,
    docs: Map[String, Package] = Map.empty
  ) = new MarkdownPage(
    path,
    stringToSource(path, sourceCode),
    params,
    includes,
    docs
  )

  private def htmlPage(
    sourceCode: String,
    path: String = "test-page",
    params: Map[String, AnyRef] = Map.empty,
    includes: Map[String, Include] = Map.empty,
    docs: Map[String, Package] = Map.empty
  ) = new HtmlPage(
    path,
    stringToSource(path, sourceCode),
    params,
    includes
  )

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

    assertEquals("<p>great</p>\n", page.html)
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

    assertEquals("<p>Hello, world!</p>\n", page1.html)

    val page2 = markdownPage(
      """|{{ content }}""".stripMargin,
      params = Map("content" -> "hello")
    )
    assert(
      page2.yaml == Map(),
      s"""incorrect yaml, expected "key:" without key in: ${page2.yaml}"""
    )
    assertEquals("<p>hello</p>\n", page2.html)

    val page3 = markdownPage(
      """|{% if product.title == "Awesome Shoes" %}
         |These shoes are awesome!
         |{% endif %}""".stripMargin,
      params = Map("product" -> Map("title" -> "Awesome Shoes").asJava)
    )

    assertEquals(
      "<p>These shoes are awesome!</p>\n",
      page3.html
    )
  }

  @Test def simpleHtmlPage = {
    val p1 = htmlPage("""<h1>{{ "hello, world!" }}</h1>""")
    assert(p1.yaml == Map(), "non-empty yaml found")
    assertEquals("<h1>hello, world!</h1>", p1.html)
  }

  @Test def htmlPageHasNoYaml = {
    val page = htmlPage(
      """|---
         |layout: main
         |---
         |
         |Hello, world!""".stripMargin
    )

    assert(!page.html.contains("---\nlayout: main\n---"),
           s"page still contains yaml:\n${page.html}")
  }

  @Test def illegalYamlFrontMatter = try {
    val page = htmlPage(
      """|---
         |layout: main
         |
         |
         |Hello, world!""".stripMargin
    )

    page.html
    fail("illegal front matter didn't throw exception")
  } catch {
    case IllegalFrontMatter(x) => // success!
    case t: Throwable => throw t
  }
}
