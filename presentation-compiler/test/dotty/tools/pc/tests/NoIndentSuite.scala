package dotty.tools.pc.tests

import java.nio.file.Path

import dotty.tools.pc.base.{BaseCompletionSuite, BaseExtractMethodSuite}

import org.junit.Ignore
import org.junit.Test

class ExtractMethodNoIndentSuite extends BaseExtractMethodSuite:
  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    List("-no-indent")

  @Test def `simple-expr` =
    checkEdit(
      s"""|object A{
          |  val b = 4
          |  def method(i: Int) = i + 1
          |  @@val a = <<123 + method(b)>>
          |}""".stripMargin,
      s"""|object A{
          |  val b = 4
          |  def method(i: Int) = i + 1
          |  def newMethod(): Int =
          |    123 + method(b)
          |
          |  val a = newMethod()
          |}""".stripMargin
    )

  @Test def `multiple-expr` =
    checkEdit(
      s"""|object A {
          |  @@val a = {
          |    val c = 1
          |    <<val b = {
          |      val c = 2
          |      c + 1
          |    }
          |    c + 2>>
          |  }
          |}""".stripMargin,
      s"""|object A {
          |  def newMethod(c: Int): Int = {
          |    val b = {
          |      val c = 2
          |      c + 1
          |    }
          |    c + 2
          |  }
          |  val a = {
          |    val c = 1
          |    newMethod(c)
          |  }
          |}""".stripMargin
    )

class CompletionMatchNoIndentSuite extends BaseCompletionSuite:
  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    List("-no-indent")

  @Test def `basic` =
    checkEdit(
      s"""
         |object A {
         |  Option(1) match@@
         |}""".stripMargin,
      s"""
         |object A {
         |  Option(1) match {
         |\tcase$$0
         |}
         |}""".stripMargin,
      filter = !_.contains("exhaustive")
    )

  @Ignore
  @Test def `exhaustive` =
    checkEdit(
      s"""
         |object A {
         |  Option(1) match@@
         |}""".stripMargin,
      s"""
         |object A {
         |  Option(1) match {
         |\tcase None => $$0
         |\tcase Some(value) =>
         |}
         |}""".stripMargin,
      filter = _.contains("exhaustive")
    )
