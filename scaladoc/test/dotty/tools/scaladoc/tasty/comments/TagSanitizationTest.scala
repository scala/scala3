package dotty.tools.scaladoc
package tasty
package comments

import org.junit.Test
import org.junit.Assert.*

import java.nio.file.Files

class TagSanitizationTest extends BaseHtmlTest:
  private def docHtml(cls: String): String =
    super.docHtml("sanitization", cls, "markdown")

  @Test def scriptTag(): Unit =
    val html = docHtml("Script")
    assertEquals("<p>alert('hello')</p>", html)

  @Test def scriptTagWithSpaces(): Unit =
    val html = docHtml("ScriptWithSpaces")
    assertEquals("<p>&lt; script &gt;alert('hello')</p>", html)

  @Test def scriptTagWithSafeChar(): Unit =
    val html = docHtml("FakeSafeScript")
    assertEquals("<p>alert('hello')</p>", html)

  @Test def notATag(): Unit =
    val html = docHtml("NotATag")
    assertEquals("<p>Example &lt; Second &lt;: Third &lt;= Fourth</p>", html)

  @Test def notATagButHasGreaterThan(): Unit =
    val html = docHtml("NotATagButHasGreaterThan")
    assertEquals("<p>Example &lt; Second &gt;: Third</p>", html)

  @Test def notATagButNoSpaces(): Unit =
    val html = docHtml("NotATagButNoSpaces")
    assertEquals("<p>a&lt;b</p>", html)

  @Test def tagOutsideCode(): Unit =
    val html = docHtml("TagOutsideCode")
    assertFalse(html, html.contains("<script>"))
    assertFalse(html, html.contains("</script>"))

  @Test def linkToTagLike(): Unit =
    val html = docHtml("LinkToTagLike")
    // ensure we don't treat the text between <:< and >:> as a tag content
    assertTrue(html, html.contains("or"))
