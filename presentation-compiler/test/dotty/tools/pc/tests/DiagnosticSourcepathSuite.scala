package dotty.tools.pc.tests

import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.CancelToken
import scala.meta.pc.SourcePathMode
import scala.meta.pc.VirtualFileParams

import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.base.DiagnosticTestHelpers
import dotty.tools.pc.utils.TestExtensions.getOffset

import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.junit.Test

class DiagnosticSourcepathSuite extends BasePCSuite with DiagnosticTestHelpers:
  private val sourcepathDir: Path = Files.createDirectories(tmp.resolve("sourcepath"))

  def getDiagnostics(text: String): List[Diagnostic] =
    presentationCompiler
      .didChange(
        TestVirtualFileParams(URI.create("file:/Diagnostic.scala"), text)
      )
      .get()
      .asScala
      .toList

  locally:
    val pkg1Dir = Files.createDirectories(sourcepathDir.resolve("pkg1"))
    Files.write(
      pkg1Dir.resolve("Alpha.scala"),
      """|package pkg1
         |
         |class Alpha:
         |  def greetAlpha: String = ""
         |  val countAlpha: Int = 0
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    val pkg2Dir = Files.createDirectories(sourcepathDir.resolve("pkg2"))
    Files.write(
      pkg2Dir.resolve("WithError.scala"),
      """|package pkg2
         |
         |class WithError:
         |  val greeting: String = 42
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )

  override protected def config: PresentationCompilerConfigImpl =
    super.config.copy(sourcePathMode = SourcePathMode.PRUNED)
  override protected val sourcePath: Seq[Path] = Seq(sourcepathDir)

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    Seq("-Ylogical-package-loading")

  @Test def `uses-class-from-sourcepath-without-error` =
    check(
      """|import pkg1.Alpha
         |object Main:
         |  val a = new Alpha
         |  val greeting: String = a.greetAlpha
         |""".stripMargin,
      List()
    )

  @Test def `type-mismatch-using-sourcepath-class` =
    check(
      """|import pkg1.Alpha
         |object Main:
         |  val a = new Alpha
         |  val count: String = a.countAlpha
         |""".stripMargin,
      List(
        TestDiagnostic(
          73,
          85,
          "Found:    (Main.a.countAlpha : Int)\nRequired: String",
          DiagnosticSeverity.Error
        )
      )
    )

  @Test def `uses-class-with-error-from-sourcepath` =
    check(
      """|import pkg2.WithError
         |object Main:
         |  val w = new WithError
         |  val greeting: String = w.greeting
         |""".stripMargin,
      List()
    )
