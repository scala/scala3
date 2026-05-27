package dotty.tools.pc.tests.completion

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.PresentationCompilerConfig.OverrideDefFormat

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionOverrideConfigSuite extends BaseCompletionSuite:

  override def config: PresentationCompilerConfigImpl =
    super.config.copy(
      _symbolPrefixes = Map(
        "a/Weekday." -> "w",
        "java/util/function/" -> "f"
      ),
      overrideDefFormat = OverrideDefFormat.Unicode
    )

  @Test def `object` =
    checkEdit(
      """|package a
         |object Weekday {
         |  case class Monday()
         |}
         |class Super {
         |  def weekday: Weekday.Monday
         |}
         |class Main extends Super {
         |  def weekday@@
         |}
         |""".stripMargin,
      """|package a
         |
         |import a.{Weekday => w}
         |object Weekday {
         |  case class Monday()
         |}
         |class Super {
         |  def weekday: Weekday.Monday
         |}
         |class Main extends Super {
         |  def weekday: w.Monday = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `package` =
    checkEdit(
      """|package b
         |class Package {
         |  def function: java.util.function.Function[Int, String]
         |}
         |class Main extends Package {
         |  def function@@
         |}
         |""".stripMargin,
      """|package b
         |
         |import java.util.{function => f}
         |class Package {
         |  def function: java.util.function.Function[Int, String]
         |}
         |class Main extends Package {
         |  def function: f.Function[Int, String] = ${0:???}
         |}
         |""".stripMargin
    )

  @Test def `unicode` =
    check(
      """|package c
         |class Number {
         |  def number: Int = 42
         |  def numberAbstract: Int
         |}
         |class Main extends Number {
         |  def number@@
         |}
         |""".stripMargin,
      """|🔼 def numberAbstract: Int
         |⏫ override def number: Int
         |""".stripMargin
    )
