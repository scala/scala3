package dotty.tools.scaladoc
package signatures

import scala.jdk.CollectionConverters._
import java.nio.file.Path
import org.jsoup.Jsoup
import util.IO
import org.junit.{Test, Assert}
import Assert._

/** Tests the markup driving the interactive capture checking toggle:
 *  cc-specific signature fragments must be wrapped in `.feature-cc` spans
 *  holding a `.feature-on` variant and, where the plain rendering differs
 *  (e.g. function arrows), a `.feature-off` variant, and pages must offer
 *  the toggle button.
 */
class FeatureToggleTest extends ScaladocTest("captureCheckingSignatures"):

  private def htmlFiles(op: org.jsoup.nodes.Document => Unit)(using DocContext): Unit =
    val output = summon[DocContext].args.output.nn.toPath
    IO.foreachFileIn(output, path =>
      if path.toString.endsWith(".html") then op(Jsoup.parse(IO.read(path))))

  @Test
  def ccFragmentsAreToggleable(): Unit = afterRendering {
    var toggles = 0
    var arrowToggles = 0
    htmlFiles { doc =>
      doc.select(".feature-cc").asScala.foreach { elem =>
        toggles += 1
        val on = elem.select("> .feature-on")
        assertEquals(s"expected exactly one .feature-on variant in ${elem.outerHtml}", 1, on.size)
        val onText = on.text
        val offText = elem.select("> .feature-off").text
        // A cc arrow must fall back to the plain impure arrow when toggled off
        if onText.startsWith("->") then
          arrowToggles += 1
          assertEquals(s"wrong fallback for cc arrow in ${elem.outerHtml}", "=>", offText)
        else if onText.startsWith("?->") then
          arrowToggles += 1
          assertEquals(s"wrong fallback for cc arrow in ${elem.outerHtml}", "?=>", offText)
      }
    }
    assertTrue("expected cc-toggleable fragments in the rendered docs", toggles > 0)
    assertTrue("expected cc arrows with a plain fallback in the rendered docs", arrowToggles > 0)
  }

  @Test
  def ccToggleButtonIsRendered(): Unit = afterRendering {
    var pages = 0
    htmlFiles { doc =>
      if !doc.select("#header").isEmpty then
        pages += 1
        assertEquals(s"expected a cc toggle button on ${doc.title}", 1, doc.select("#cc-toggle").size)
        assertEquals(s"expected a mobile cc toggle on ${doc.title}", 1, doc.select("#mobile-cc-toggle").size)
    }
    assertTrue("expected framed pages in the rendered docs", pages > 0)
  }

/** With -suppressCC, even capture-checked sources must render without any
 *  toggle machinery: no cc fragments, no toggle buttons, no cc syntax.
 */
class SuppressedFeatureToggleTest extends ScaladocTest("captureCheckingSignatures"):
  override def args = super.args.copy(suppressCC = true)

  @Test
  def noCcMarkupWhenSuppressed(): Unit = afterRendering {
    val output = summon[DocContext].args.output.nn.toPath
    IO.foreachFileIn(output, path =>
      if path.toString.endsWith(".html") then {
        val doc = Jsoup.parse(IO.read(path))
        assertTrue(s"unexpected cc fragments in $path", doc.select(".feature-cc").isEmpty)
        assertTrue(s"unexpected cc toggle button in $path", doc.select("#cc-toggle").isEmpty)
        assertTrue(s"unexpected mobile cc toggle in $path", doc.select("#mobile-cc-toggle").isEmpty)
        // No cc syntax may leak into signatures as plain text either
        val signatures = doc.select(".signature").text
        assertFalse(s"cc arrow leaked into suppressed signatures in $path", signatures.contains("->"))
        assertFalse(s"capture set leaked into suppressed signatures in $path", signatures.contains("^{"))
        assertFalse(s"uses clause leaked into suppressed signatures in $path", signatures.contains(" uses "))
      })
  }

/** Without any capture-checked sources the cc toggle button must not appear. */
class NoFeatureToggleTest extends ScaladocTest("classSignatureTestSource"):

  @Test
  def noCcToggleButton(): Unit = afterRendering {
    val output = summon[DocContext].args.output.nn.toPath
    IO.foreachFileIn(output, path =>
      if path.toString.endsWith(".html") then {
        val doc = Jsoup.parse(IO.read(path))
        assertTrue(s"unexpected cc toggle button in $path", doc.select("#cc-toggle").isEmpty)
        assertTrue(s"unexpected cc fragments in $path", doc.select(".feature-cc").isEmpty)
      })
  }
