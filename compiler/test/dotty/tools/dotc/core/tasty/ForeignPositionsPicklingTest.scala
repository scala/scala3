package dotty.tools.dotc.core.tasty

import java.nio.file.{Files, Path}

import org.junit.Test
import org.junit.Assert.{assertArrayEquals, assertFalse, assertTrue}

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

  // `export` copies the annotations of the exported members, and of their
  // parameters, onto the forwarders unchanged.
  private val exportParent =
    """package p
      |class foo(x: Int) extends scala.annotation.StaticAnnotation
      |class Svc:
      |  @foo(3) def run(@foo(4) n: Int): Int = n
      |  @deprecated("s", "1") def old: Int = 2
      |  @foo(5) val v: Int = 1
      |  @foo(6) var w: Int = 1
      |  @foo(7) def poly[@foo(8) T](t: T): T = t
      |  @foo(9) given gv: Ordering[Int] = Ordering.Int
      |  @foo(10) type A = Int
      |  @foo(11) def curried(@foo(12) a: Int)(@foo(13) b: Int): Int = a + b
      |  extension (@foo(14) s: String) @foo(15) def ext: Int = 1
      |""".stripMargin

  private val exportChild =
    """package p
      |class E1:
      |  val svc = new Svc
      |  export svc.{run, old}
      |class E2:
      |  val svc = new Svc
      |  export svc.{v, w}
      |class E3:
      |  val svc = new Svc
      |  export svc.poly
      |class E4:
      |  val svc = new Svc
      |  export svc.{gv, A}
      |class E5:
      |  val svc = new Svc
      |  export svc.{curried, ext}
      |""".stripMargin

  private val exportChildClasses = List("E1", "E2", "E3", "E4", "E5")

  private def compile(parentPrefix: String): List[Array[Byte]] =
    compile(parentPrefix, parent, child, childClasses)

  private def compile(parentPrefix: String, parent: String, child: String, childClasses: List[String]): List[Array[Byte]] =
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

  @Test def parentLayoutDoesNotAffectExportForwarderTasty(): Unit =
    val before = compile("", exportParent, exportChild, exportChildClasses)
    val after = compile("// a comment\n\n", exportParent, exportChild, exportChildClasses)
    val differing = for (c, b, a) <- exportChildClasses.lazyZip(before).lazyZip(after).toList
                        if !java.util.Arrays.equals(b, a) yield c
    assertTrue(s"TASTy depends on the layout of P.scala for: ${differing.mkString(", ")}", differing.isEmpty)
}
