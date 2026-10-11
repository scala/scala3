package dotty.tools.pc.tests.completion

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.SemanticdbFileManager
import scala.meta.pc.SourcePathMode

import dotty.tools.dotc.Main
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.requiredClass
import dotty.tools.pc.CachingDriver
import dotty.tools.pc.RawScalaPresentationCompiler
import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.base.TestResources

import org.eclipse.lsp4j.CompletionTriggerKind
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/** Regression: a class that exists both as TASTy on the classpath and as a
 *  `.scala` file on the sourcepath used to be entered twice. Completing a
 *  *different* buffer that refers to that class forces `SourcefileLoader` and
 *  used to crash in `SetRootTree`. Typing the dual file itself is not enough:
 *  namer replaces the source-loaded symbols before they are completed.
 */
class DualClasspathSourcepathSuite:
  private val tmp = Files.createTempDirectory("dual-classpath-sourcepath")
  private val sourcepathDir: Path = Files.createDirectories(tmp.resolve("sourcepath-and-cp"))
  private val classesDir: Path = Files.createDirectories(tmp.resolve("classes-and-cp"))
  private val alphaSrc: Path =
    Files.createDirectories(sourcepathDir.resolve("pkg1").resolve("pkg2")).resolve("Alpha.scala")

  private val alphaSource: String =
    """|package pkg1
       |package pkg2
       |
       |type Id[A] = A
       |
       |class Alpha:
       |  def greetAlpha: Id[String] = ""
       |
       |object Alpha:
       |  def empty: Alpha = new Alpha
       |""".stripMargin

  locally:
    Files.write(alphaSrc, alphaSource.getBytes(StandardCharsets.UTF_8))
    val classpath = TestResources.classpath.mkString(File.pathSeparator)
    val reporter = Main.process(Array(
      "-d",
      classesDir.toString,
      "-classpath",
      classpath,
      alphaSrc.toString,
    ))
    assertFalse("Failed to compile sourcepath class onto the classpath", reporter.hasErrors)

  // Metals sends individual source files, not just the root directory.
  protected val sourcePath: Seq[Path] = Seq(alphaSrc)
  protected val additionalClasspath: Seq[Path] = Seq(classesDir)

  private def driverSettings: List[String] =
    val classpath = (TestResources.classpath ++ additionalClasspath).mkString(File.pathSeparator)
    List(
      "-classpath",
      classpath,
      "-sourcepath",
      alphaSrc.toString,
      "-Ylogical-package-loading",
      "-color:never",
    )

  private def newDriver(): CachingDriver =
    CachingDriver(
      driverSettings,
      () => sourcePath.asJava,
      SemanticdbFileManager.EMPTY,
      SourcePathMode.PRUNED,
    )

  private val clientCode: String =
    """|package client
       |
       |import pkg1.pkg2.{Alpha, Id}
       |
       |object Main:
       |  val a: Id[Alpha] = Alpha.empty
       |  a.greetAlpha
       |""".stripMargin

  @Test def `classpath-class-is-not-reloaded-from-sourcepath` =
    val driver = newDriver()
    driver.run(URI.create("file:/Main.scala"), clientCode)
    given Context = driver.currentCtx
    val alpha = requiredClass("pkg1.pkg2.Alpha")
    // Force the loader. Without the merge this late-compiles Alpha.scala and
    // used to crash in SetRootTree; with the merge it unpickles TASTy.
    alpha.info
    val file = alpha.associatedFile
    assertTrue(
      s"Alpha should be loaded from TASTy on the classpath, not recompiled from sourcepath. Got: $file",
      file != null && (file.ext.isTasty || file.ext.isClass),
    )
