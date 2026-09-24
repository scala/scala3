package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionAmmoniteSuite extends BaseCompletionSuite:

  @Test def `version-sort-empty` =
    checkSubset(
      """|import $ivy.`com.lihaoyi::pprint:@@`
         |""".stripMargin,
      """|0.7.3
         |0.7.2
         |0.7.1
         |0.7.0
         |""".stripMargin,
      filename = "A.worksheet.sc",
      enablePackageWrap = false
    )

  private def checkSubset(
      original: String,
      expected: String,
      filename: String,
      enablePackageWrap: Boolean
  ) =
    val expectedAtLeast = expected.linesIterator.toSet
    check(
      original,
      expected,
      filter = expectedAtLeast,
      filename = filename,
      enablePackageWrap = enablePackageWrap
    )
