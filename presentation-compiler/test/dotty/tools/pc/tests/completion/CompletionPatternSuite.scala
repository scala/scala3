package dotty.tools.pc.tests.completion

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionPatternSuite extends BaseCompletionSuite:

  def paramHint: Option[String] = Some("param-hint")

  override def config: PresentationCompilerConfigImpl =
    super.config.copy(
      _parameterHintsCommand = paramHint
    )

  @Test def `empty` =
    checkEdit(
      """
        |object A {
        |  Option(1) match {
        |    case @@ =>
        |  }
        |}""".stripMargin,
      """
        |object A {
        |  Option(1) match {
        |    case Some(value)$0 =>
        |  }
        |}""".stripMargin,
      filter = _.contains("Some(value)")
    )

  @Test def `ident` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case S@@ =>
        |  }
        |}""".stripMargin,
      """|Some(value) scala
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `bind` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case ma@@
        |  }
        |}""".stripMargin,
      ""
    )

  @Test def `bind2` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case abc @ @@ =>
        |  }
        |}""".stripMargin,
      """|None scala
         |Some(value) scala
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `bind-ident` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case abc @ S@@ =>
        |  }
        |}""".stripMargin,
      """|Some(value) scala
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `wildcard` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case _: @@ =>
        |  }
        |}""".stripMargin,
      """|Some[?] scala
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `wildcard-ident` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case _: S@@ =>
        |  }
        |}""".stripMargin,
      """|Some[?] scala
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `typed-bind` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case ab: @@ =>
        |  }
        |}""".stripMargin,
      """|Some[?] scala
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `typed-bind-ident` =
    check(
      """
        |object A {
        |  Option(1) match {
        |    case ab: S@@ =>
        |  }
        |}""".stripMargin,
      """|Some[?] scala
         |""".stripMargin,
      topLines = Some(1)
    )
