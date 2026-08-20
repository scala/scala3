package dotty.tools.scaladoc
package tasty.comments

import org.junit.Test
import org.junit.Assert.{assertFalse, assertTrue}

import dotty.tools.scaladoc.tasty.comments.markdown.DocFlexmarkRenderer

class FootnoteSupportTest {
  @Test def renderMarkdownFootnotes(): Unit = {
    val markdown =
      """Top-level splices are restricted to inline methods[^1].
        |
        |[^1]: See the metaprogramming reference for details.
        |""".stripMargin

    val html = DocFlexmarkRenderer.render(MarkdownParser.parseToMarkdown(markdown))((_, _) => "")

    assertTrue(s"expected a footnote reference in: $html", html.contains("footnote-ref"))
    assertTrue(s"expected a footnote definition block in: $html", html.contains("class=\"footnotes\""))
    assertTrue(s"expected the footnote text in: $html", html.contains("See the metaprogramming reference for details."))
    assertFalse(s"footnote markers should not render literally in: $html", html.contains("[^1]"))
  }
}
