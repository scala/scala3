package dotty.tools.pc.tests.edit

import dotty.tools.pc.base.BaseAutoImportsSuite

import org.junit.Test

class AutoImportExtensionMethodsSuite extends BaseAutoImportsSuite:

  @Test def `basic` =
    check(
      """|object A:
         |  extension (num: Int) def incr = ???
         |
         |def main = 1.<<incr>>
         |""".stripMargin,
      """|A
         |""".stripMargin
    )

  @Test def `basic-edit` =
    checkEdit(
      """|object A:
         |  extension (num: Int) def incr = ???
         |
         |def main = 1.<<incr>>
         |""".stripMargin,
      """|import A.incr
         |object A:
         |  extension (num: Int) def incr = ???
         |
         |def main = 1.incr
         |""".stripMargin
    )
