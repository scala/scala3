package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionFilenameSuit extends BaseCompletionSuite:

  @Test def `class` =
    check(
      """|
         |class M@@
         |""".stripMargin,
      "class Main",
      filename = "Main.scala"
    )

  @Test def `companion-class` =
    check(
      """|object Main
         |class M@@
         |""".stripMargin,
      "class Main",
      filename = "Main.scala"
    )

  @Test def `companion-trait` =
    check(
      """|object Main
         |trait M@@
         |""".stripMargin,
      "trait Main",
      filename = "Main.scala"
    )

  @Test def `companion-object` =
    check(
      """|trait TestTrait
         |object T@@
         |""".stripMargin,
      "object TestTrait",
      filename = "TestTrait.scala"
    )

  @Test def `companion-object2` =
    check(
      """|class TestClass
         |object TestC@@
         |""".stripMargin,
      "object TestClass",
      filename = "TestClass.scala"
    )

  @Test def `duplicate` =
    check(
      """|object Duplicate
         |class Duplicate
         |class Du@@
         |""".stripMargin,
      "",
      filename = "Duplicate.scala"
    )

  @Test def `inner` =
    check(
      """|object Outer {
         |  class M@@
         |}
         |""".stripMargin,
      "",
      filename = "Main.scala"
    )

  @Test def `fuzzy` =
    check(
      """|
         |class MDataSer@@
         |""".stripMargin,
      "class MyDatabaseService",
      filename = "MyDatabaseService.scala"
    )

  @Test def `path` =
    check(
      """|
         |class Us@@
         |""".stripMargin,
      "class User",
      filename = "foo/User.scala"
    )

  @Test def `object` =
    check(
      """|
         |object TestUs@@
         |""".stripMargin,
      "object TestUser",
      filename = "TestUser.scala"
    )

  @Test def `trait` =
    check(
      """|
         |trait Us@@
         |""".stripMargin,
      "trait User",
      filename = "User.scala"
    )

  @Test def `type-duplicate` =
    check(
      """|
         |class User
         |trait Us@@
         |""".stripMargin,
      "",
      filename = "User.scala"
    )

  @Test def `term-duplicate` =
    check(
      """|
         |object User
         |object Us@@
         |""".stripMargin,
      "",
      filename = "User.scala"
    )

  @Test def `end-of-file` =
    check(
      "object Use@@",
      "object User",
      filename = "User.scala"
    )
