package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

/** SIP-80: `#` companion shorthand completions in target-typed positions.
 *
 *  Tests use `#X@@` (at least one identifier char after the hash) so the
 *  parser successfully produces a `HashSelect` and the typer rewrites it to
 *  `Select(companionRef, X)`. The bare `#@@` form (cursor immediately after a
 *  hash with no identifier char) does not parse and therefore offers no
 *  completions in this v1 — users get suggestions once they type any letter.
 */
class HashCompanionCompletionsSuite extends BaseCompletionSuite:

  private val colorAdt =
    """|sealed trait Color
       |object Color:
       |  case object Red   extends Color
       |  case object Blue  extends Color
       |  case object Green extends Color
       |""".stripMargin

  @Test def `val-rhs-prefix` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |val c: Color = #R@@
          |""".stripMargin,
      "Red: Red",
      filter = _.startsWith("Red")
    )

  @Test def `val-rhs-empty-prefix-letter` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |val c: Color = #B@@
          |""".stripMargin,
      "Blue: Blue",
      filter = _.startsWith("Blue")
    )

  @Test def `arg-position` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |def paint(c: Color): String = c.toString
          |val s: String = paint(#R@@)
          |""".stripMargin,
      "Red: Red",
      filter = _.startsWith("Red")
    )

  @Test def `named-arg` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |def paint(c: Color): String = c.toString
          |val s: String = paint(c = #G@@)
          |""".stripMargin,
      "Green: Green",
      filter = _.startsWith("Green")
    )

  @Test def `pattern-position` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |val c: Color = Color.Red
          |val s: String = c match
          |  case #R@@ => "red"
          |  case _    => "other"
          |""".stripMargin,
      "Red: Red",
      filter = _.startsWith("Red")
    )

  @Test def `pattern-named-extractor` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |final case class Wrap(c: Color)
          |val w: Wrap = Wrap(Color.Red)
          |val s: String = w match
          |  case Wrap(c = #R@@) => "red"
          |  case _              => "other"
          |""".stripMargin,
      "Red: Red",
      filter = _.startsWith("Red")
    )

  @Test def `enum-cases` =
    check(
      """|import scala.language.experimental.hashCompanionShorthand
         |enum Light:
         |  case On, Off, Dim
         |val l: Light = #O@@
         |""".stripMargin,
      """|On: Light
         |Off: Light""".stripMargin,
      filter = label => label.startsWith("On") || label.startsWith("Off")
    )

  @Test def `null-union` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |val c: Color | Null = #R@@
          |""".stripMargin,
      "Red: Red",
      filter = _.startsWith("Red")
    )

  @Test def `null-union-reversed` =
    check(
      s"""|import scala.language.experimental.hashCompanionShorthand
          |$colorAdt
          |val c: Null | Color = #B@@
          |""".stripMargin,
      "Blue: Blue",
      filter = _.startsWith("Blue")
    )

  @Test def `no-import-no-completions` =
    // Without the language import the parser rejects `#X`, so the hash
    // completer should not contribute anything specific to SIP-80.
    check(
      """|sealed trait Color
         |object Color:
         |  case object Red extends Color
         |val c: Color = #R@@
         |""".stripMargin,
      "",
      filter = _.startsWith("Red")
    )
