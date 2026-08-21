package dotty.tools.scaladoc
package tasty.comments

import org.junit.Test
import org.junit.Assert.{assertEquals, assertFalse, assertTrue}

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

  @Test def separateAdjacentFootnoteReferences(): Unit = {
    val markdown =
      """Restricted to inline methods[^1][^2], but not[^1] elsewhere[^2].
        |
        |[^1]: First footnote.
        |[^2]: Second footnote.
        |""".stripMargin

    val html = DocFlexmarkRenderer.render(MarkdownParser.parseToMarkdown(markdown))((_, _) => "")

    val separators = "footnote-ref-sep".r.findAllIn(html).size
    assertEquals(s"only the adjacent references should be separated in: $html", 1, separators)
    assertTrue(s"expected the separator between adjacent references in: $html",
      html.contains("""</sup><sup class="footnote-ref-sep">,</sup><sup"""))
  }
}
