package dotty.tools.pc.tests.completion

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

import scala.collection.immutable
import scala.language.unsafeNulls
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.SourcePathMode

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionSourcepathSuite extends BaseCompletionSuite:
  private val sourcepathDir: Path = Files.createDirectories(tmp.resolve("sourcepath"))
  private val singleSourcepathDir: Path = Files.createDirectories(tmp.resolve("sourcepath2"))
  private val singleSourcepathFile = singleSourcepathDir.resolve("Gamma.scala")

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
      pkg2Dir.resolve("Beta.scala"),
      """|package pkg2.pkg3.pkg4
         |
         |object Beta:
         |  def greetBeta(name: String): String = s"Hello $name"
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    val pkg3Dir = Files.createDirectories(sourcepathDir.resolve("pkg3"))
    Files.write(
      pkg3Dir.resolve("toplevel.scala"),
      """|package pkg3
         |
         |def greetFromToplevel(name: String): String = s"Hello $name"
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    Files.write(
      singleSourcepathFile,
      """|package pkg1
         |package pkg2
         |
         |class Gamma:
         |  def greetGamma: String = ""
         |  val countAlpha: Int = 0
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )

  override protected def config: PresentationCompilerConfigImpl =
    super.config.copy(sourcePathMode = SourcePathMode.FULL)
  override protected val sourcePath: Seq[Path] = Seq(sourcepathDir, singleSourcepathFile)

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    Seq("-Ylogical-package-loading")

  @Test def `class-from-sourcepath` =
    check(
      """|import pkg1.Alpha
         |object Main:
         |  val a = new Alpha
         |  a.greetA@@
         |""".stripMargin,
      """|greetAlpha: String
         |""".stripMargin
    )

  @Test def `object-from-different-package-in-sourcepath` =
    check(
      """|import pkg2.pkg3.pkg4.Beta
         |object Main:
         |  Beta.greetB@@
         |""".stripMargin,
      """|greetBeta(name: String): String
         |""".stripMargin
    )

  @Test def `object-from-sourcefile` =
    check(
      """|import pkg1.pkg2.Gamma
         |
         |object ObjectFromSourceFile:
         |  val gamma = new Gamma
         |  gamma.greet@@
         |""".stripMargin,
      """|greetGamma: String
         |""".stripMargin
    )

  @Test def `toplevel-from-sourcepath` =
    check(
      """|import pkg3.greetFromToplevel
         |object Main:
         |  greetFromToplevel@@
         |""".stripMargin,
      """|greetFromToplevel(name: String): String
         |""".stripMargin
    )
