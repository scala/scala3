package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Ignore
import org.junit.Test

class CompletionScalaCliSuite extends BaseCompletionSuite:

  @Test def `simple` =
    checkSubset(
      """|//> using lib "io.cir@@
         |package A
         |""".stripMargin,
      """|io.circe
         |io.circul""".stripMargin
    )

  @Test def `simple-lib-name` =
    checkSubset(
      """|//> using dep "org.scala-lang::scala@@
         |package A
         |""".stripMargin,
      """|scala3-library
         |""".stripMargin,
    )

  @Test def `ordering` =
    check(
      """|//> using dep "org.scala@@
         |package A
         |""".stripMargin,
      """|org.scala-debugger
         |org.scala-lang
         |org.scala-lang-osgi
         |""".stripMargin,
      filter = a => a.contains("scala-lang") || a.contains("scala-debugger")
    )

  @Test def `multiple-deps` =
    checkEdit(
      """|// Multiple using lib
         |//> using lib ???
         |// //> using lib ???
         |//> using lib io.circe::circe-core_na@@
         |package A
         |""".stripMargin,
      """|// Multiple using lib
         |//> using lib ???
         |// //> using lib ???
         |//> using lib io.circe::circe-core_native0.4
         |package A
         |""".stripMargin,
      assertSingleItem = false,
      filter = _.contains("circe-core_native0.4")
    )

  @Test def `version-sort` =
    checkSubset(
      """|//> using dep "com.lihaoyi::pprint:0.7@@"
         |package A
         |""".stripMargin,
      """|0.7.3
         |0.7.2
         |0.7.1
         |0.7.0
         |""".stripMargin
    )

  @Ignore
  @Test def `single-colon` =
    checkSubset(
      """|//> using lib "io.circe:circe-core_na@@
         |package A
         |""".stripMargin,
      """|circe-core_native0.4_2.12
         |circe-core_native0.4_2.13
         |circe-core_native0.4_3
         |""".stripMargin
    )

  @Test def `version` =
    checkSubset(
      """|//> using lib "io.circe::circe-core_sjs1:0.14.10@@"
         |package A
         |""".stripMargin,
      "0.14.10"
    )

  // We don't to add `::` before version if `sjs1` is specified
  @Test def `version-edit` =
    checkEdit(
      """|//> using lib "io.circe::circe-core_sjs1:0.14.10@@"
         |package A
         |""".stripMargin,
      """|//> using lib "io.circe::circe-core_sjs1:0.14.10"
         |package A
         |""".stripMargin,
      filter = _.endsWith("0.14.10")
    )

  @Ignore
  @Test def `multiple-libs` =
    checkSubset(
      """|//> using lib "io.circe::circe-core:0.14.0", "io.circe::circe-core_na@@"
         |package A
         |""".stripMargin,
      "circe-core_native0.4"
    )

  @Ignore
  @Test def `script` =
    checkSubset(
      scriptWrapper(
        """|//> using lib "io.circe:circe-core_na@@
           |
           |""".stripMargin,
        "script.sc.scala"
      ),
      """|circe-core_native0.4_2.12
         |circe-core_native0.4_2.13
         |circe-core_native0.4_3
         |""".stripMargin,
      filename = "script.sc.scala",
      enablePackageWrap = false
    )

  @Test def `closing-quote` =
    checkSubset(
      """|//> using lib "io.circe::circe-core:0.14.0"@@
         |package A
         |""".stripMargin,
      ""
    )

  @Test def `whitespace` =
    checkSubset(
      """|//> using lib "io.circe::circe-co @@
         |package A
         |""".stripMargin,
      ""
    )

  @Test def `alternative-sorting` =
    checkEdit(
      """|//> using lib "co.fs2::fs2-core:@@"
         |package A
         |""".stripMargin,
      """|//> using lib "co.fs2::fs2-core::3.4.0"
         |package A
         |""".stripMargin,
      filter = _.startsWith("3.4")
    )

  @Test def `dep` =
    checkSubset(
      """|//> using dep "io.cir@@
         |package A
         |""".stripMargin,
      """|io.circe
         |io.circul""".stripMargin
    )

  @Test def `lexicographic-order` =
    checkSubset(
      """|//> using dep "org.scala-lang@@"
         |package A
         |""".stripMargin,
      """|org.scala-lang
         |org.scala-lang-osgi
         |""".stripMargin
    )

  @Ignore
  @Test def `multiple-deps2` =
    checkSubset(
      """|//> using libs "io.circe::circe-core:0.14.0", "io.circe::circe-core_na@@"
         |package A
         |""".stripMargin,
      "circe-core_native0.4"
    )

  def checkSubset(
      original: String,
      expected: String,
      filename: String = "A.scala",
      enablePackageWrap: Boolean = true
  ) =
    val expectedAtLeast = expected.linesIterator.toSet
    check(
      original,
      expected,
      filter = expectedAtLeast,
      filename = filename,
      enablePackageWrap = enablePackageWrap
    )

  private def scriptWrapper(code: String, filename: String): String =
    // Vaguely looks like a scala file that ScalaCLI generates
    // from a sc file.
    s"""|
        |object ${filename.stripSuffix(".sc.scala")} {
        |/*<script>*/${code}
        |}
        |""".stripMargin
