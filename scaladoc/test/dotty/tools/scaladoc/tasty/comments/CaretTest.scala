package dotty.tools.scaladoc
package tasty
package comments

import org.junit.Test
import org.junit.Assert._
import java.nio.file.Files

/** Test that `<sup>` tags render correctly in both wiki and markdown modes,
  * and that `^...^` in wiki mode creates a `<sup>...</sup>`.
  *
  * See https://github.com/scala/scala3/issues/25517
  */
class CaretTest extends BaseHtmlTest:

  private def docHtml(cls: String, syntax: String): String =
    super.docHtml("i25517", cls, syntax)

  @Test def supTagsInMarkdown(): Unit =
    val html = docHtml("SupDefault", "markdown")
    assertEquals("<p>2<sup>29</sup> 2<sup>30</sup></p>", html)

  @Test def supTagsInWiki(): Unit =
    val html = docHtml("SupWiki", "wiki")
    assertTrue(html.contains("2<sup>29</sup>"))
    assertTrue(html.contains("2<sup>30</sup>"))

  @Test def bareCaretBrokenInWiki(): Unit =
    val html = docHtml("CaretWiki", "wiki")
    assertTrue(html.contains("<sup>"))

  @Test def pairedCaretInWiki(): Unit =
    val html = docHtml("PairedCaretWiki", "wiki")
    assertTrue(html.contains("2<sup>29</sup>"))
    assertTrue(html.contains("2<sup>30</sup>"))
