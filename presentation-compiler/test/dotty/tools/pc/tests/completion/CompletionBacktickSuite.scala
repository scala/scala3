package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionBacktickSuite extends BaseCompletionSuite:

  @Test def `keyword` =
    check(
      """|object Main {
         |  val `type` = 42
         |  Main.typ@@
         |}
         |""".stripMargin,
      "type: Int",
      filterText = "type"
    )

  @Test def `keyword-edit` =
    checkEdit(
      """|object Main {
         |  val `type` = 42
         |  Main.typ@@
         |}
         |""".stripMargin,
      """|object Main {
         |  val `type` = 42
         |  Main.`type`
         |}
         |""".stripMargin,
      filterText = "type"
    )

  @Test def `space` =
    check(
      s"""|object Main {
          |  val `hello world` = 42
          |  Main.hello@@
          |}
          |""".stripMargin,
      "hello world: Int",
      filterText = "hello world"
    )

  @Test def `comment` =
    check(
      s"""|object Main {
          |  val `///` = 42
          |  Main./@@
          |}
          |""".stripMargin,
      "///: Int",
      filterText = "///"
    )

  @Test def `normal` =
    check(
      """|object Main {
         |  val `spaced` = 42
         |  spaced@@
         |}
         |""".stripMargin,
      // NOTE(olafur) expected output is not backticked because the compiler symbol does not
      // distinguish if the symbol was defined with backticks in source.
      """spaced: Int
        |""".stripMargin,
      filterText = ""
    )

  @Test def `negative` =
    check(
      """|object Main {
         |  val `type` = 42
         |  Main.`typ@@
         |}
         |""".stripMargin,
      // NOTE(olafur) expected output is empty because the source does not tokenize due to unclosed identifier.
      // It would be nice to fix this limitation down the road.
      "",
      filter = _.contains("`type`")
    )

  // https://nightly.scala-lang.org/docs/internals/syntax.html#soft-keywords
  @Test def `soft-keywords-check` =
    List(
      "infix",
      "inline",
      "opaque",
      "open",
      "transparent",
      "as",
      "derives",
      "end",
      "extension",
      "throws",
      "using"
    ).foreach(softKeywordCheck)

  private def softKeywordCheck(keyword: String) =
    checkEdit(
      s"""|object Main {
          |  def $keyword(a: String) = a
          |  ${keyword}@@
          |}
          |""".stripMargin,
      s"""|object Main {
          |  def $keyword(a: String) = a
          |  `$keyword`($$0)
          |}
          |""".stripMargin,
      filter = _.contains("a: String")
    )

  @Test def `soft-keyword-select` =
    checkEdit(
      """|object Main {
         |  case class Pos(start: Int, end: Int)
         |  val a = Pos(1,2)
         |  val b = a.end@@
         |}
         |""".stripMargin,
      """|object Main {
         |  case class Pos(start: Int, end: Int)
         |  val a = Pos(1,2)
         |  val b = a.end
         |}
         |""".stripMargin
    )

  @Test def `soft-keyword-ident` =
    checkEdit(
      """|object Main {
         |  case class Pos(start: Int, end: Int) {
         |    val point = start - end@@
         |  }
         |}
         |""".stripMargin,
      """|object Main {
         |  case class Pos(start: Int, end: Int) {
         |    val point = start - `end`
         |  }
         |}
         |""".stripMargin
    )

  @Test def `soft-keyword-extension` =
    checkEdit(
      """|object A {
         |  extension (a: String) def end = a.last
         |}
         |object Main {
         |  val a = "abc".end@@
         |}
         |""".stripMargin,
      """|import A.end
         |object A {
         |  extension (a: String) def end = a.last
         |}
         |object Main {
         |  val a = "abc".end
         |}
         |""".stripMargin,
      filter = _.contains("end: Char")
    )

  @Test def `keyword-select` =
    checkEdit(
      """|object Main {
         |  case class Pos(start: Int, `lazy`: Boolean)
         |  val a = Pos(1,true)
         |  val b = a.laz@@
         |}
         |""".stripMargin,
      """|object Main {
         |  case class Pos(start: Int, `lazy`: Boolean)
         |  val a = Pos(1,true)
         |  val b = a.`lazy`
         |}
         |""".stripMargin
    )

  @Test def `add-backticks-around-identifier` =
    checkEdit(
      """|object Main {
         |  def `Foo Bar` = 123
         |  Foo@@
         |}
         |""".stripMargin,
      """|object Main {
         |  def `Foo Bar` = 123
         |  `Foo Bar`
         |}
         |""".stripMargin
    )

  @Test def `complete-inside-backticks` =
    checkEdit(
      """|object Main {
         |  def `Foo Bar` = 123
         |  `Foo@@`
         |}
         |""".stripMargin,
      """|object Main {
         |  def `Foo Bar` = 123
         |  `Foo Bar`
         |}
         |""".stripMargin
    )

  @Test def `complete-inside-backticks-after-space` =
    checkEdit(
      """|object Main {
         |  def `Foo Bar` = 123
         |  `Foo B@@a`
         |}
         |""".stripMargin,
      """|object Main {
         |  def `Foo Bar` = 123
         |  `Foo Bar`
         |}
         |""".stripMargin
    )

  @Test def `complete-inside-empty-backticks` =
    checkEdit(
      """|object Main {
         |  def `Foo Bar` = 123
         |  `@@`
         |}
         |""".stripMargin,
      """|object Main {
         |  def `Foo Bar` = 123
         |  `Foo Bar`
         |}
         |""".stripMargin,
      filter = _ == "Foo Bar: Int"
    )
