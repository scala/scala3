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

  private def docHtml(cls: String, syntax: String = "markdown"): String =
    val dest = Files.createTempDirectory("test-doc").toFile
    try
      val args = Scaladoc.Args(
        name = projectName,
        tastyFiles = tastyFiles("i25517"),
        output = dest,
        projectVersion = Some(projectVersion),
        defaultSyntax = List(syntax),
      )
      Scaladoc.run(args)(using testContext)
      val path = dest.toPath.resolve(s"tests/i25517/$cls.html")
      val doc = org.jsoup.Jsoup.parse(dotty.tools.scaladoc.util.IO.read(path))
      doc.select(".doc").html()
    finally dotty.tools.scaladoc.util.IO.delete(dest)

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
