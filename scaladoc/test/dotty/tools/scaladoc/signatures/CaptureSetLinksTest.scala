package dotty.tools.scaladoc
package signatures

import scala.jdk.CollectionConverters._
import org.jsoup.Jsoup
import util.IO
import org.junit.{Test, Assert}
import Assert._

/** References in capture sets and uses clauses link to the documentation of
 *  classes and their members; parameters and root capabilities stay plain.
 */
class CaptureSetLinksTest extends ScaladocTest("captureCheckingSignatures"):

  @Test
  def capabilityLinks(): Unit = afterRendering {
    val output = summon[DocContext].args.output.nn.toPath
    def links(page: String, selector: String): Seq[String] =
      val doc = Jsoup.parse(IO.read(output.resolve(s"tests/captureCheckingSignatures/$page.html")))
      doc.select(s"$selector a").asScala.filterNot(a => a.hasClass("documentableName") || a.attr("t") == "n").map(_.text).toSeq
    // `impureAny2: Int ->{any, a, b, c} Int`, where `a`, `b` and `c` are members of `Arrows`
    val members = links("Arrows", ".documentableElement[id^=impureAny2-] .signature")
    assertTrue(s"expected links to a, b, c in $members", Set("a", "b", "c").subsetOf(members.toSet))
    assertFalse(s"unexpected link to the root capability in $members", members.contains("any"))
    // `uncurried: (x: AnyRef^, y: AnyRef^) -> AnyRef^{x, y} => Int ->{x, y} Int`
    val params = links("Arrows", ".documentableElement[id^=uncurried-] .signature")
    assertFalse(s"unexpected link to a parameter in $params", params.contains("x") || params.contains("y"))
    // `class UsesShort uses UsesExamples.this.io`
    val uses = links("UsesExamples$UsesShort", ".main-signature")
    assertTrue(s"expected links to UsesExamples and io in $uses", Set("UsesExamples", "io").subsetOf(uses.toSet))
  }
