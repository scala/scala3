package dotty.tools.dotc.core.tasty

import java.nio.file.{Files, Path}

import org.junit.Test
import org.junit.Assert.{assertArrayEquals, assertFalse}

import dotty.tools.dotc.Main
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting.TestReporter

import dotty.tools.vulpix.TestConfiguration

/** The TASTy of a class must not depend on the layout of another source file,
 *  even if it contains annotated types that were copied from that file.
 */
class ForeignPositionsPicklingTest {

  private val parent =
    """package p
      |class foo(x: Int) extends scala.annotation.StaticAnnotation
      |abstract class P {
      |  def f: Int @foo(1)
      |  def tell(m: Any)(implicit s: P = null): Unit
      |}
      |object PO {
      |  def g: Int @foo(2) = 1
      |  def k(f: (Int @foo(3)) => Int): Int = 0
      |}
      |class G[T] { def g: T @foo(4) = ??? }
      |""".stripMargin

  // Each class gets an annotated type from `P.scala` by a different route.
  private val child =
    """package p
      |// result types inferred by `Namer`: an inherited result type, the
      |// default getter of `tell` (`P @uncheckedVariance`), and a selection
      |class Q extends P {
      |  def f = 1
      |  def tell(m: Any)(implicit s: P = null): Unit = ()
      |  val y = PO.g
      |}
      |// the type of a local definition
      |class R1 { def n: Unit = { val l = PO.g; () } }
      |// an inferred type argument
      |class R2 { def n: Unit = { List(PO.g); () } }
      |// `asSeenFrom`
      |class R3 { def n: Unit = { val l = new G[String].g; () } }
      |// a lambda parameter typed from the expected type
      |class R4 { val v: Int = PO.k(x => x) }
      |""".stripMargin

  private val childClasses = List("Q", "R1", "R2", "R3", "R4")

  private def compile(parentPrefix: String): List[Array[Byte]] =
    val dir = Files.createTempDirectory("foreign-positions")
    val src = Files.createDirectory(dir.resolve("src"))
    val out = Files.createDirectory(dir.resolve("out"))
    Files.writeString(src.resolve("P.scala"), parentPrefix + parent)
    Files.writeString(src.resolve("Q.scala"), child)
    val options = TestConfiguration.defaultOptions
      .and("-d", out.toString)
      .and("-sourceroot", src.toString)
      .and(src.resolve("P.scala").toString, src.resolve("Q.scala").toString)
    val reporter = TestReporter.reporter(System.out, logLevel = ERROR)
    assertFalse("Compilation failed.", Main.process(options.all, reporter).hasErrors)
    childClasses.map(c => Files.readAllBytes(out.resolve(s"p/$c.tasty")))

  @Test def parentLayoutDoesNotAffectChildTasty(): Unit =
    val before = compile("")
    val after = compile("// a comment\n\n")
    for (c, b, a) <- childClasses.lazyZip(before).lazyZip(after) do
      assertArrayEquals(s"$c.tasty", b, a)
}
