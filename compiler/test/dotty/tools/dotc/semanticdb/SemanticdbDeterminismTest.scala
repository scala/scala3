package dotty.tools.dotc.semanticdb

import java.nio.file.{Files, Path}

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.dotc.Main
import dotty.tools.vulpix.TestConfiguration

/** SemanticDB output must not depend on hash codes, which differ from one compile to the
 *  next in the same JVM (`Name`s hash by identity). Refinements and lambda parameters of
 *  polymorphic function types are numbered as `localN` through `TypeOps.fakeSymbols`.
 */
class SemanticdbDeterminismTest:

  private val source =
    """object O:
      |  val v = [T] => (y: T) => (x: y.type) => 3
      |  val u = [T] => (y: T) => (x: y.type) => 3
      |  def m = v(u)
      |  type F = [X] => (x: X, y: Int) => X
      |  val f: F = [X] => (x: X, y: Int) => x
      |""".stripMargin

  private def compile(src: Path): Array[Byte] =
    val out = Files.createTempDirectory("semanticdb-determinism")
    val reporter = Main.process(Array(
      "-Xsemanticdb", "-sourceroot", src.getParent.toString, "-semanticdb-target", out.toString,
      "-d", out.toString, "-classpath", TestConfiguration.basicClasspath, src.toString))
    assertFalse("compilation failed", reporter.hasErrors)
    Files.readAllBytes(out.resolve("META-INF/semanticdb/O.scala.semanticdb"))

  @Test def sameOutputWhenCompiledRepeatedly: Unit =
    val dir = Files.createTempDirectory("semanticdb-determinism-src")
    val src = Files.writeString(dir.resolve("O.scala"), source)
    val first = compile(src)
    for i <- 1 to 10 do
      assertArrayEquals(s"compile #${i + 1} differs from the first", first, compile(src))
