package dotty.tools
package dotc

import core.Comments.docCtx
import core.Contexts.*
import vulpix.TestConfiguration

import org.junit.Test
import org.junit.Assert.*

import java.nio.file.Files
import scala.collection.mutable.ArrayBuffer

/** A long-lived driver (REPL, IDE, compile server) reuses one root context
 *  for many runs. State that a run leaves in the root context retains that run
 *  (trees, symbols, contexts) for as long as the driver lives, and can leak into
 *  later runs.
 */
class MultiRunRetentionTest:

  private val sources = List(
    "A.scala" ->
      """package p
        |
        |/** A class with a doc comment. */
        |class A:
        |  /** A method with a doc comment. */
        |  def m: Int = 1
        |
        |enum Expr[T]:
        |  case IntLit(i: Int) extends Expr[Int]
        |  case BoolLit(b: Boolean) extends Expr[Boolean]
        |
        |/** GADT pattern matching records constraints in the context's GadtState. */
        |def eval[T](e: Expr[T]): T = e match
        |  case Expr.IntLit(i) => i
        |  case Expr.BoolLit(b) => b
        |
        |sealed trait These[+A, +B]
        |object These:
        |  final case class This[A](a: A) extends These[A, Nothing]
        |  final case class Both[A, B](a: A, b: B) extends These[A, B]
        |  def unapply[A, B](t: Both[A, B]): Some[(A, B)] = Some((t.a, t.b))
        |
        |/** The exhaustivity check of a custom generic `unapply` adds
        | *  pattern-bound type symbols to the GadtState of its context. */
        |def size[A, B](t: These[A, B]): Int = t match
        |  case These.This(_) => 1
        |  case These(_, _) => 2
        |""".stripMargin,
    // Must come after another file of the package: indexing that file brings the
    // previous run's package object forward to the new run before it is replaced.
    "package.scala" ->
      """/** A package object with a doc comment. */
        |package object p:
        |  /** A type alias with a doc comment, in a package object. */
        |  type Num = Int
        |""".stripMargin
  )

  @Test def noStateFromEarlierRuns(): Unit =
    val dir = Files.createTempDirectory("multi-run-retention")
    val out = Files.createDirectories(dir.resolve("out"))
    val files = for (name, content) <- sources yield
      Files.writeString(dir.resolve(name), content).toString

    object driver extends Driver:
      var lastRun: Run | Null = null
      override protected def newCompiler(using Context): Compiler = new Compiler:
        override def newRun(using Context): Run =
          val run = super.newRun
          lastRun = run
          run
      def compileRepeatedly(n: Int): Unit =
        val args = Array("-classpath", TestConfiguration.basicClasspath, "-d", out.toString) ++ files
        val Some((sourceFiles, rootCtx)) = setup(args, initCtx.fresh): @unchecked
        val compiler = newCompiler(using rootCtx)
        val docstringCounts = ArrayBuffer.empty[Int]
        for _ <- 1 to n do
          val reporter = doCompile(compiler, sourceFiles)(using rootCtx)
          assertFalse(reporter.hasErrors)
          // Compiling must not record GADT constraints in the shared root context,
          // nor in the run context, which is shared by all compilation units.
          assertEquals(Nil, rootCtx.gadt.symbols)
          assertEquals(Nil, lastRun.nn.runContext.gadt.symbols)
          docstringCounts += rootCtx.docCtx.get.docstrings.size
          // Docstrings of permanent symbols, here a synthetic one, survive the pruning.
          assertTrue(rootCtx.docCtx.get.docstring(rootCtx.definitions.orType).isDefined)
        // The docstrings of symbols replaced by a later run must be dropped.
        assertEquals(List.fill(n)(docstringCounts.head), docstringCounts.toList)

    driver.compileRepeatedly(4)
