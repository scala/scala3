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
    assertEquals("<p>alert('hello')</p>", html)

  @Test def scriptTagWithSafeChar(): Unit =
    val html = docHtml("FakeSafeScript")
    assertEquals("<p>alert('hello')</p>", html)