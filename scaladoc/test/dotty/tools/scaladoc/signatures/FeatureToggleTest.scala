package dotty.tools.scaladoc
package signatures

import scala.jdk.CollectionConverters._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import util.IO
import org.junit.{Test, Assert}
import Assert._

object FeatureToggleTest:
  def htmlFiles(op: (String, Document) => Unit)(using DocContext): Unit =
    val output = summon[DocContext].args.output.nn.toPath
    IO.foreachFileIn(output, path =>
      if path.toString.endsWith(".html") then op(path.toString, Jsoup.parse(IO.read(path))))

  /** A page must offer the cc switch iff it contains cc fragments. Returns
   *  whether the page has the switch.
   */
  def checkCcSwitch(path: String, doc: Document): Boolean =
    val hasFragments = !doc.select(".feature-cc").isEmpty
    val switches = doc.select("input[data-feature-toggle=cc]").size
    assertEquals(s"expected a cc switch iff there are cc fragments in $path", if hasFragments then 1 else 0, switches)
    hasFragments

  /** Checks the signatures readers see with cc hidden: no cc syntax may be
   *  left, and the toggled fragments must not unbalance brackets. Returns
   *  the hidden texts of the signatures.
   */
  def checkNoCcSyntaxWhenHidden()(using DocContext): Seq[String] =
    val texts = Seq.newBuilder[String]
    htmlFiles { (path, doc) =>
      doc.select(".feature-cc > .feature-on").remove()
      doc.select(".signature, .main-signature").asScala.foreach { sig =>
        val text = sig.text
        texts += text
        assertFalse(s"cc arrow visible with cc hidden in $path: $text", text.contains("->"))
        assertFalse(s"capture set visible with cc hidden in $path: $text", text.contains("^"))
        val keywords = sig.select("[t=k]").asScala.map(_.text.trim)
        for kw <- List("uses", "consume", "update") do
          assertFalse(s"cc keyword $kw visible with cc hidden in $path: $text", keywords.contains(kw))
        for (open, close) <- List('(' -> ')', '[' -> ']', '{' -> '}') do
          assertEquals(s"unbalanced $open$close with cc hidden in $path: $text", text.count(_ == open), text.count(_ == close))
      }
    }
    val result = texts.result()
    assertTrue("expected signatures in the rendered docs", result.nonEmpty)
    result

import FeatureToggleTest.*

/** Tests the markup driving the interactive capture checking toggle:
 *  cc-specific signature fragments must be wrapped in `.feature-cc` spans
 *  holding a `.feature-on` variant and, where the plain rendering differs
 *  (e.g. function arrows), a `.feature-off` variant, and pages with such
 *  fragments must offer a switch toggling them.
 */
class FeatureToggleTest extends ScaladocTest("captureCheckingSignatures"):

  @Test
  def ccFragmentsAreToggleable(): Unit = afterRendering {
    var toggles = 0
    var arrowToggles = 0
    htmlFiles { (_, doc) =>
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
  def noCcSyntaxWhenHidden(): Unit = afterRendering {
    checkNoCcSyntaxWhenHidden()
  }

  @Test
  def ccFragmentsHaveHints(): Unit = afterRendering {
    htmlFiles { (path, doc) =>
      for on <- doc.select(".feature-cc > .feature-on").asScala do
        assertFalse(s"no hover text for ${on.outerHtml} in $path", on.attr("title").isEmpty)
    }
  }

  @Test
  def ccSwitchIsRendered(): Unit = afterRendering {
    var pages = 0
    htmlFiles { (path, doc) => if checkCcSwitch(path, doc) then pages += 1 }
    assertTrue("expected pages with the cc switch", pages > 0)
  }

/** The cc renderings fixed in #27232 must also have a plain variant. */
class CCRenderingFeatureToggleTest extends ScaladocTest("ccRendering"):

  @Test
  def noCcSyntaxWhenHidden(): Unit = afterRendering {
    val texts = checkNoCcSyntaxWhenHidden()
    for expected <- List("def pureFS(fs: FS): Unit", "val polyCapturing: [A] => A => Int") do
      assertTrue(s"expected a signature containing `$expected` with cc hidden", texts.exists(_.contains(expected)))
  }

/** With -suppress-cc, even capture-checked sources must render without any
 *  toggle machinery: no cc fragments, no switches, no cc syntax.
 */
class SuppressedFeatureToggleTest extends ScaladocTest("captureCheckingSignatures"):
  override def args = super.args.copy(suppressCC = true)

  @Test
  def noCcMarkupWhenSuppressed(): Unit = afterRendering {
    htmlFiles { (path, doc) =>
      assertTrue(s"unexpected cc fragments in $path", doc.select(".feature-cc").isEmpty)
      assertTrue(s"unexpected feature switch in $path", doc.select("[data-feature-toggle], .feature-toggles").isEmpty)
      // No cc syntax may leak into signatures as plain text either
      val signatures = doc.select(".signature").text
      assertFalse(s"cc arrow leaked into suppressed signatures in $path", signatures.contains("->"))
      assertFalse(s"capture set leaked into suppressed signatures in $path", signatures.contains("^{"))
      assertFalse(s"uses clause leaked into suppressed signatures in $path", signatures.contains(" uses "))
    }
  }

/** Sources without capture checking only get the cc switch on pages showing
 *  members inherited from capture-checked classes (here, the standard library's
 *  `Seq`), since signatures are rendered according to their defining class.
 */
class InheritedFeatureToggleTest extends ScaladocTest("classSignatureTestSource"):

  @Test
  def ccSwitchOnlyWithInheritedCcMembers(): Unit = afterRendering {
    var withSwitch = 0
    var withoutSwitch = 0
    htmlFiles { (path, doc) =>
      if checkCcSwitch(path, doc) then withSwitch += 1 else withoutSwitch += 1
    }
    assertTrue("expected pages with members inherited from capture-checked classes", withSwitch > 0)
    assertTrue("expected pages without cc fragments", withoutSwitch > 0)
  }
