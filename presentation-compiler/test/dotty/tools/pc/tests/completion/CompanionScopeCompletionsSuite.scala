package dotty.tools.pc.tests.completion

import java.nio.file.Path

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompanionScopeCompletionsSuite extends BaseCompletionSuite:

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    List("-language:experimental.companionScopeInference")

  @Test def `val-rhs` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  val c: Color = R@@
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `arg-position` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  def paint(c: Color) = ???
         |  val _ = paint(R@@)
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `named-arg` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  def paint(c: Color) = ???
         |  val _ = paint(c = R@@)
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `pattern-position` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  def describe(c: Color): String = c match
         |    case R@@
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `enum-cases` =
    check(
      """|object O:
         |  enum Light:
         |    case On, Off, Dim
         |  val l: Light = O@@
         |""".stripMargin,
      """|On: Light
         |Off: Light
         |""".stripMargin,
      filter = s => s.startsWith("On") || s.startsWith("Off")
    )

  @Test def `null-union` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  val c: Color | Null = R@@
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `null-union-flipped` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  val c: Null | Color = R@@
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `option-target` =
    check(
      """|object O:
         |  enum Color:
         |    case Red, Green, Blue
         |  val c: Option[Color] = Some(R@@)
         |""".stripMargin,
      """|Red: Color
         |""".stripMargin,
      filter = _.startsWith("Red")
    )

  @Test def `opaque-alias` =
    check(
      """|object O:
         |  opaque type Level = Int
         |  object Level:
         |    val Info: Level = 1
         |    val Bad: Level = 2
         |  val l: Level = I@@
         |""".stripMargin,
      """|Info: Level
         |""".stripMargin,
      filter = _ == "Info: Level"
    )

  @Test def `no-companion` =
    check(
      """|object O:
         |  def m(f: Int => String) = ???
         |  val _ = m(N@@)
         |""".stripMargin,
      "",
      filter = _.startsWith("Nothing")
    )

end CompanionScopeCompletionsSuite
