package dotty.tools.scaladoc
package tasty
package comments

import org.junit.Test
import org.junit.Assert.*

import java.nio.file.Files

class TagSanitizationTest extends BaseHtmlTest:
  private def docHtml(cls: String): String =
    super.docHtml("sanitization", cls, "markdown")

  @Test def simpleScriptTag(): Unit =
    val html = docHtml("Simple")
    assertEquals("&lt;script&gt;alert('hello')&lt;script&gt;", html)