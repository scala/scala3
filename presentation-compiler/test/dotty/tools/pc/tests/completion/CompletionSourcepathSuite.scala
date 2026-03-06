package dotty.tools.pc.tests.completion

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import scala.collection.immutable
import scala.language.unsafeNulls

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionSourcepathSuite extends BaseCompletionSuite:

  val sourcepathDir: Path = Files.createDirectories(tmp.resolve("sourcepath"))
   // TODO no package test

  // Write source files to the sourcepath directory under different packages
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

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    Seq("-sourcepath", sourcepathDir.toString, "-Yprune-sourcepath")

  // @Test def `class-from-sourcepath` =
  //   check(
  //     """|import pkg1.Alpha
  //        |object Main:
  //        |  val a = new Alpha
  //        |  a.greetA@@
  //        |""".stripMargin,
  //     """|greetAlpha: String
  //        |""".stripMargin
  //   )

  @Test def `object-from-different-package-in-sourcepath` =
    check(
      """|import pkg2.pkg3.pkg4.Beta
         |object Main:
         |  Beta.greetB@@
         |""".stripMargin,
      """|greetBeta(name: String): String
         |""".stripMargin
    )
