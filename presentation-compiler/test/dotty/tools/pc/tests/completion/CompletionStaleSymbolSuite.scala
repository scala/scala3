package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionStaleSymbolSuite extends BaseCompletionSuite:

  @Test def `multiple-requests` =
    checkRenamedToplevel(
      "Metals",
      (contents, max) => contents.replace("object Metals", s"object Metals$max"),
      s"""|B example
          |Metals123456789 example
          |""".stripMargin
    )

  @Test def `multiple-requests-backtick` =
    checkRenamedToplevel(
      "`M Metals M`",
      (contents, max) => contents.replace("Metals M", "Metals M "),
      s"""|B example
          |M Metals M example
          |""".stripMargin
    )

  private def checkRenamedToplevel(
      toRename: String,
      renameFunc: (String, Int) => String,
      expected: String
  ): Unit =
    val file1 = "A.scala"
    val file2 = "B.scala"
    val baseFile = s"""|package example
                       |object $toRename {
                       |  val x = 1
                       |  x@@
                       |}""".stripMargin

    def loop(fileContents: String, max: Int = 9): Unit =
      getItems(fileContents, file1)
      if max > 0 then
        loop(renameFunc(fileContents, max), max - 1)

    loop(baseFile)

    val items = getItems(
      s"""|package example
          |object B {
          |  val x = 1
          |  example.@@
          |}""".stripMargin,
      file2
    )
    assertNoDiff(
      items.map(_.getLabel).mkString("\n"),
      expected
    )
