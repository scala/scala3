package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import staticsite.MarkdownPage

class YamlTest extends DottyDocTest {
  import scala.collection.JavaConverters._

  @Test def has1Key = {
    val page = new MarkdownPage(
      """|---
         |key:
         |---
         |
         |great""".stripMargin,
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
       Map("content" -> "Hello, world!")
    )

    assert(
      page1.yaml == Map("key" -> ""),
      s"""incorrect yaml, expected "key:" without key in: ${page1.yaml}"""
    )

    assertEquals("<p>Hello, world!</p>\n", page1.html)

    val page2 = new MarkdownPage(
      """|{{ content }}""".stripMargin,
      Map("content" -> "hello")
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
      Map("product" -> Map("title" -> "Awesome Shoes").asJava)
    )

    assertEquals(
      "<p>These shoes are awesome!</p>\n",
      page3.html
    )
  }
}
