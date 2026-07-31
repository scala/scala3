package dotty.tools.dotc.semanticdb

import org.junit.Assert.*
import org.junit.Test

import dotty.tools.dotc.Main
import dotty.tools.nio.*
import dotty.tools.vulpix.TestConfiguration

import scala.io.Codec

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

  private def compile(src: File): Array[Byte] =
    val out = FileContainer.createTemporaryOnDisk("semanticdb-determinism")
    val reporter = Main.process(Array(
      "-Xsemanticdb", "-sourceroot", src.parent.path, "-semanticdb-target", out.path,
      "-d", out.path, "-classpath", TestConfiguration.basicClasspath, src.path))
    assertFalse("compilation failed", reporter.hasErrors)
    out.getOrCreateFile("META-INF/semanticdb/O.scala.semanticdb").readBytes()

  @Test def sameOutputWhenCompiledRepeatedly: Unit =
    val dir = FileContainer.createTemporaryOnDisk("semanticdb-determinism-src")
    val src = dir.getOrCreateFile("O.scala")
    src.writeText(source, Codec.UTF8)
    val first = compile(src)
    for i <- 1 to 10 do
      assertArrayEquals(s"compile #${i + 1} differs from the first", first, compile(src))
