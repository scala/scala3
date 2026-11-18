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
  private val flatSourcepathDir: Path = Files.createDirectories(tmp.resolve("sourcepath3"))

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
    val nestedPkgDir = Files.createDirectories(sourcepathDir.resolve("a").resolve("b"))
    Files.write(
      nestedPkgDir.resolve("package.scala"),
      """|package a.b
         |
         |export a.lib.Helpers.*
         |export a.lib.Lilaism.{ *, given }
         |
         |def greetFromParentPackage(name: String): String = s"Hello $name"
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    Files.write(
      nestedPkgDir.resolve("ParentModel.scala"),
      """|package a.b
         |
         |class ParentModel:
         |  def modelField: Int = 0
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    val libDir = Files.createDirectories(sourcepathDir.resolve("a").resolve("lib"))
    Files.write(
      libDir.resolve("Helpers.scala"),
      """|package a.lib
         |
         |object Helpers:
         |  def helperFromExport(x: Int): Int = x
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    Files.write(
      libDir.resolve("Lilaism.scala"),
      """|package a.lib
         |
         |object Lilaism:
         |  type LilaId = String
         |  given lilaOrdering: Ordering[LilaId] = Ordering.String
         |  extension (id: LilaId) def lilaShort: String = id.take(3)
         |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    )
    Files.write(
      flatSourcepathDir.resolve("package.scala"),
      """|package flat.module
         |
         |def greetFromFlatModule(name: String): String = s"Hello $name"
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
    super.config.copy(sourcePathMode = SourcePathMode.PRUNED)
  override protected val sourcePath: Seq[Path] = Seq(sourcepathDir, singleSourcepathFile, flatSourcepathDir)

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

  @Test def `package-object-from-enclosing-package-clause` =
    check(
      """|package a.b
         |package c
         |
         |object Main:
         |  greetFromParent@@
         |""".stripMargin,
      """|greetFromParentPackage(name: String): String
         |""".stripMargin
    )

  @Test def `export-from-enclosing-package-clause` =
    check(
      """|package a.b
         |package c
         |
         |object Main:
         |  helperFromEx@@
         |""".stripMargin,
      """|helperFromExport(x: Int): Int
         |""".stripMargin
    )

  @Test def `exported-type-and-extension-from-enclosing-package-clause` =
    check(
      """|package a.b
         |package c
         |
         |object Main:
         |  val id: LilaId = "abcdef"
         |  id.lilaSh@@
         |""".stripMargin,
      """|lilaShort: String
         |""".stripMargin
    )

  @Test def `class-from-other-file-in-enclosing-package-clause` =
    check(
      """|package a.b
         |package c
         |
         |object Main:
         |  val model = new ParentModel
         |  model.modelF@@
         |""".stripMargin,
      """|modelField: Int
         |""".stripMargin
    )

  @Test def `package-object-not-matching-directory-from-enclosing-package-clause` =
    check(
      """|package flat.module
         |package ui
         |
         |object Main:
         |  greetFromFlat@@
         |""".stripMargin,
      """|greetFromFlatModule(name: String): String
         |""".stripMargin
    )
