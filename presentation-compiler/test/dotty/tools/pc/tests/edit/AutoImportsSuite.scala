package dotty.tools.pc.tests.edit

import dotty.tools.pc.base.BaseAutoImportsSuite

import org.junit.Test

class AutoImportsSuite extends BaseAutoImportsSuite:

  @Test def `basic` =
    check(
      """|object A {
         |  <<Future>>.successful(2)
         |}
         |""".stripMargin,
      """|scala.concurrent
         |""".stripMargin
    )

  @Test def `basic-apply` =
    check(
      """|object A {
          |  <<Future>>(2)
          |}
          |""".stripMargin,
      """|scala.concurrent
         |""".stripMargin
    )

  @Test def `basic-function-apply` =
    check(
      """|
          |object ForgeFor{
          |  def importMe(): Int = ???
          |}
          |object ForgeFor2{
          |  case class importMe()
          |}
          |
          |
          |object test2 {
          |  <<importMe>>()
          |}
          |""".stripMargin,
      """|ForgeFor2
         |ForgeFor
         |""".stripMargin
    )

  @Test def `basic-apply-wrong` =
    check(
      """|object A {
          |  new <<Future>>(2)
          |}
          |""".stripMargin,
      """|scala.concurrent
         |java.util.concurrent
         |""".stripMargin
    )

  @Test def `basic-fuzzy` =
    check(
      """|object A {
          |  <<Future>>.thisMethodDoesntExist(2)
          |}
          |""".stripMargin,
      """|scala.concurrent
         |java.util.concurrent
         |""".stripMargin
    )

  @Test def `typed-simple` =
    check(
      """|object A {
          |  import scala.concurrent.Promise
          |  val fut: <<Future>> = Promise[Unit]().future
          |}
          |""".stripMargin,
      """|scala.concurrent
         |java.util.concurrent
         |""".stripMargin
    )

  @Test def `basic-edit` =
    checkEdit(
      """|package a
         |
         |object A {
         |  <<Future>>.successful(2)
         |}
         |""".stripMargin,
      """|package a
         |
         |import scala.concurrent.Future
         |
         |object A {
         |  Future.successful(2)
         |}
         |""".stripMargin
    )

  @Test def `basic-edit-comment` =
    checkEdit(
      """|/**
         | * @param code
         | * @return
         |*/
         |object A {
         |  <<Future>>.successful(2)
         |}
         |""".stripMargin,
      """|import scala.concurrent.Future
         |/**
         | * @param code
         | * @return
         |*/
         |object A {
         |  Future.successful(2)
         |}
         |""".stripMargin
    )

  @Test def `basic-edit-directive` =
    checkEdit(
      """|// using scala 35
         |// using something else
         |
         |object A {
         |  <<Future>>.successful(2)
         |}
         |""".stripMargin,
      """|// using scala 35
         |// using something else
         |import scala.concurrent.Future
         |
         |object A {
         |  Future.successful(2)
         |}
         |""".stripMargin
    )

  @Test def `scala-cli-sc-using-directives` =
    checkEdit(
      """|object main {
         |/*<script>*///> using scala "3.1.3"
         |
         |object x {
         |  <<Try>>("1".toString)
         |}
         |}
         |
         |""".stripMargin,
      """|object main {
         |/*<script>*///> using scala "3.1.3"
         |import scala.util.Try
         |
         |object x {
         |  Try("1".toString)
         |}
         |}
         |""".stripMargin,
      filename = "A.sc.scala"
    )

  @Test def `symbol-no-prefix` =
    checkEdit(
      """|package a
         |
         |object A {
         |  val uuid = <<UUID>>.randomUUID()
         |}
         |""".stripMargin,
      """|package a
         |
         |import java.util.UUID
         |
         |object A {
         |  val uuid = UUID.randomUUID()
         |}
         |""".stripMargin
    )

  @Test def `symbol-prefix-existing` =
    checkEdit(
      """|package a
         |
         |object A {
         |  val uuid = <<UUID>>.randomUUID()
         |}
         |""".stripMargin,
      """|package a
         |
         |import java.util.UUID
         |
         |object A {
         |  val uuid = UUID.randomUUID()
         |}
         |""".stripMargin
    )

  @Test def `symbol-prefix` =
    checkEdit(
      """|package a
         |
         |object A {
         |  val l : <<Map>>[String, Int] = ???
         |}
         |""".stripMargin,
      """|package a
         |
         |import java.{util => ju}
         |
         |object A {
         |  val l : ju.Map[String, Int] = ???
         |}
         |""".stripMargin
    )

  @Test def `interpolator-edit` =
    checkEdit(
      """|package a
         |
         |object A {
         |  val l = s"${<<Seq>>(2)}"
         |}
         |""".stripMargin,
      """|package a
         |
         |import scala.collection.mutable
         |
         |object A {
         |  val l = s"${mutable.Seq(2)}"
         |}
         |""".stripMargin
    )

  @Test def `package-object` =
    checkEdit(
      """|
         |package object metals{
         |  object ABC
         |}
         |object Main{
         | val obj = <<ABC>>
         |}
         |""".stripMargin,
      """|import metals.ABC
         |
         |package object metals{
         |  object ABC
         |}
         |object Main{
         | val obj = ABC
         |}
         |""".stripMargin
    )

  @Test def `import-inside-package-object` =
    checkEdit(
      """|package a
         |
         |package object b {
         |  val l = s"${<<ListBuffer>>(2)}"
         |}
         |""".stripMargin,
      """|package a
         |
         |import scala.collection.mutable.ListBuffer
         |
         |package object b {
         |  val l = s"${ListBuffer(2)}"
         |}
         |""".stripMargin
    )

  @Test def `multiple-packages` =
    checkEdit(
      """|package a
         |package b
         |package c
         |
         |object A {
         |  val l = s"${<<ListBuffer>>(2)}"
         |}
         |""".stripMargin,
      """|package a
         |package b
         |package c
         |
         |import scala.collection.mutable.ListBuffer
         |
         |object A {
         |  val l = s"${ListBuffer(2)}"
         |}
         |""".stripMargin
    )

  @Test def `multiple-packages-existing-imports` =
    checkEdit(
      """|package a
         |package b
         |package c
         |
         |import scala.concurrent.Future
         |
         |object A {
         |  val l = s"${<<ListBuffer>>(2)}"
         |}
         |""".stripMargin,
      """|package a
         |package b
         |package c
         |
         |import scala.concurrent.Future
         |import scala.collection.mutable.ListBuffer
         |
         |object A {
         |  val l = s"${ListBuffer(2)}"
         |}
         |""".stripMargin
    )

  @Test def `import-in-import` =
    checkEdit(
      """|package inimport
         |
         |object A {
         |  import <<ExecutionContext>>.global
         |}
         |""".stripMargin,
      """|package inimport
         |
         |object A {
         |  import scala.concurrent.ExecutionContext.global
         |}
         |""".stripMargin
    )

  @Test def `first-auto-import-amm-script` =
    checkAmmoniteEdit(
      ammoniteWrapper(
        """|
           |val p: <<Path>> = ???
           |""".stripMargin
      ),
      ammoniteWrapper(
        """|import java.nio.file.Path
           |
           |val p: Path = ???
           |""".stripMargin
      )
    )

  @Test def `second-auto-import-amm-script` =
    checkAmmoniteEdit(
      ammoniteWrapper(
        """import java.nio.file.Files
          |val p: <<Path>> = ???
          |""".stripMargin
      ),
      ammoniteWrapper(
        """import java.nio.file.Files
          |import java.nio.file.Path
          |val p: Path = ???
          |""".stripMargin
      )
    )

  @Test def `amm-objects` =
    checkAmmoniteEdit(
      ammoniteWrapper(
        """|
           |object a {
           |  object b {
           |    val p: <<Path>> = ???
           |  }
           |}
           |""".stripMargin
      ),
      ammoniteWrapper(
        """|import java.nio.file.Path
           |
           |object a {
           |  object b {
           |    val p: Path = ???
           |  }
           |}
           |""".stripMargin
      )
    )

  @Test def `first-auto-import-amm-script-with-header` =
    checkAmmoniteEdit(
      ammoniteWrapper(
        """|// scala 2.13.1
           |
           |val p: <<Path>> = ???
           |""".stripMargin
      ),
      ammoniteWrapper(
        """|// scala 2.13.1
           |import java.nio.file.Path
           |
           |val p: Path = ???
           |""".stripMargin
      )
    )

  @Test def `worksheet-import` =
    checkWorksheetEdit(
      worksheetPcWrapper(
        """|//> using scala 3.3.0
           |
           |// Some comment
           |
           |// Object comment
           |object A {
           |  val p: <<Path>> = ???
           |}
           |""".stripMargin
      ),
      worksheetPcWrapper(
        """|//> using scala 3.3.0
           |
           |// Some comment
           |import java.nio.file.Path
           |
           |// Object comment
           |object A {
           |  val p: Path = ???
           |}
           |""".stripMargin
      )
    )

  @Test def `object-import` =
    checkEdit(
      """|object A {
         |  //some comment
         |  val p: <<Path>> = ???
         |}
         |""".stripMargin,
      """|import java.nio.file.Path
         |object A {
         |  //some comment
         |  val p: Path = ???
         |}
         |""".stripMargin
    )

  @Test def `toplevels-import` =
    checkEdit(
      """|//some comment
         |
         |val p: <<Path>> = ???
         |
         |//some other comment
         |
         |val v = 1
         |""".stripMargin,
      """|//some comment
         |import java.nio.file.Path
         |
         |val p: Path = ???
         |
         |//some other comment
         |
         |val v = 1
         |""".stripMargin
    )

  @Test def `i6477` =
    checkEdit(
      """|package a
         |import a.b.SomeClass as SC
         |
         |package b {
         |  class SomeClass
         |}
         |package c {
         |  class SomeClass
         |}
         |
         |val bar: SC = ???
         |val foo: <<SomeClass>> = ???
         |""".stripMargin,
      """|package a
         |import a.b.SomeClass as SC
         |import a.c.SomeClass
         |
         |package b {
         |  class SomeClass
         |}
         |package c {
         |  class SomeClass
         |}
         |
         |val bar: SC = ???
         |val foo: SomeClass = ???
         |""".stripMargin
    )

  @Test def `use-packages-in-scope` =
    checkEdit(
      """|import scala.collection.mutable as mut
         |
         |val l = <<ListBuffer>>(2)
         |""".stripMargin,
      """|import scala.collection.mutable as mut
         |import mut.ListBuffer
         |
         |val l = ListBuffer(2)
         |""".stripMargin
    )

  private def ammoniteWrapper(code: String): String =
    // Vaguely looks like a scala file that Ammonite generates
    // from a sc file.
    // Just not referencing any Ammonite class, that we don't pull
    // in the tests here.
    s"""|package ammonite
        |package $$file.`auto-import`
        |import _root_.scala.collection.mutable.{
        |  HashMap => MutableHashMap
        |}
        |
        |object test{
        |/*<start>*/
        |$code
        |}
        |""".stripMargin

  private def worksheetPcWrapper(code: String): String =
    s"""|object worksheet{
        |$code
        |}""".stripMargin

  // https://nightly.scala-lang.org/docs/internals/syntax.html#soft-keywords
  @Test
  def `soft-keyword-check-test` =
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
      s"""|
          |object $keyword{ object ABC }
          |object Main{ val obj = <<ABC>> }
          |""".stripMargin,
      s"""|import $keyword.ABC
          |
          |object $keyword{ object ABC }
          |object Main{ val obj = ABC }
          |""".stripMargin
    )

  @Test def scalaCliNoEmptyLineAfterDirective =
    checkEdit(
      """|//> using scala 3.5.0
         |object Main:
         |  <<Files>>
         |""".stripMargin,
      """|//> using scala 3.5.0
         |import java.nio.file.Files
         |object Main:
         |  Files
         |""".stripMargin
    )

  @Test def scalaCliNoEmptyLineAfterLicense =
    checkEdit(
      """|/**
         | * Some license text
         | */
         |
         |object Main:
         |  <<Files>>
         |""".stripMargin,
      """|/**
         | * Some license text
         | */
         |import java.nio.file.Files
         |
         |object Main:
         |  Files
         |""".stripMargin
    )

  @Test def scalaCliNoEmptyLineAfterLicenseWithPackage =
    checkEdit(
      """|/**
         | * Some license text
         | */
         |package test
         |
         |object Main:
         |  <<Files>>
         |""".stripMargin,
      """|/**
         | * Some license text
         | */
         |package test
         |
         |import java.nio.file.Files
         |
         |object Main:
         |  Files
         |""".stripMargin
    )
