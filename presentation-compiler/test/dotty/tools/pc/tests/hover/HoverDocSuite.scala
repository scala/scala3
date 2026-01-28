package dotty.tools.pc.tests.hover

import scala.meta.pc.SymbolDocumentation

import dotty.tools.pc.base.BaseHoverSuite
import dotty.tools.pc.utils.MockEntries

import org.junit.Test

class HoverDocSuite extends BaseHoverSuite:

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      ScalaMockDocumentation("java/lang/String#substring().", "substring", List(), List(MockParam("beginIndex"))),
      ScalaMockDocumentation("java/util/Collections#emptyList().", "emptyList"),
      ScalaMockDocumentation("_empty_/Alpha.apply().", "apply", List(), List(MockParam("x"))),
      ScalaMockDocumentation("_empty_/Alpha#", "init", List(), List(MockParam("x"))),
      ScalaMockDocumentation("scala/collection/LinearSeqOps#headOption().", "headOption")
    )

  @Test def `doc` =
    check(
      """object a {
        |  <<java.util.Collections.empty@@List[Int]>>
        |}
        |""".stripMargin,
      """|**Expression type**:
        |```scala
        |java.util.List[Int]
        |```
        |**Symbol signature**:
        |```scala
        |final def emptyList[T](): java.util.List[T]
        |```
        |Found documentation for java/util/Collections#emptyList().
        |""".stripMargin
    )

  @Test def `doc-parent` =
    check(
      """|object a {
         |  <<List(12).hea@@dOption>>
         |}
         |""".stripMargin,
      // Assert that the docstring is extracted.
      """|```scala
         |override def headOption: Option[Int]
         |```
         |Found documentation for scala/collection/LinearSeqOps#headOption().
         |""".stripMargin
    )

  @Test def `java-method` =
    check(
      """|object a {
         |  <<"".substri@@ng(1)>>
         |}
      """.stripMargin,
      """|```scala
         |def substring(beginIndex: Int): String
         |```
         |Found documentation for java/lang/String#substring().
         |""".stripMargin
    )

  @Test def `object` =
    check(
      """|
         |/**
         |  * Doc about object
         |  */
         |object Alpha {
         |  def apply(x: Int) = x + 1
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Int
         |```
         |Found documentation for _empty_/Alpha.apply().
         |""".stripMargin
    )

  @Test def `object1` =
    check(
      """|
         |/**
         |  * Doc about object
         |  */
         |object Alpha {
         |  /**
         |    * Doc about method
         |    */
         |  def apply(x: Int) = x + 1
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Int
         |```
         |Found documentation for _empty_/Alpha.apply().
         |""".stripMargin
    )

  @Test def `case-class` =
    check(
      """|
         |/**
         |  * Doc about case class
         |  *
         |  */
         |case class Alpha(x: Int)
         |
         |/**
         |  * Doc about object
         |  */
         |object Alpha {
         |  /**
         |    * Doc about method
         |    */
         |  def apply(x: Int) = x + 1
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Int
         |```
         |Found documentation for _empty_/Alpha.apply().
         |""".stripMargin
    )

  @Test def `case-class1` =
    check(
      """|
         |/**
         |  * Doc about case class
         |  *
         |  */
         |case class Alpha(x: Int)
         |
         |/**
         |  * Doc about object
         |  */
         |object Alpha {
         |  def apply(x: Int) = x + 1
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Int
         |```
         |Found documentation for _empty_/Alpha.apply().
         |
         |""".stripMargin
    )

  @Test def `case-class2` =
    check(
      """|
         |/**
         |  * Doc about case class
         |  *
         |  */
         |case class Alpha(x: Int)
         |
         |object Alpha {
         |  def apply(x: Int) = x + 1
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Int
         |```
         |Found documentation for _empty_/Alpha.apply().
         |""".stripMargin
    )

  @Test def `case-class3` =
    check(
      """|
         |/**
         |  * Doc about case class
         |  *
         |  */
         |case class Alpha(x: Int)
         |
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Alpha
         |```
         |Found documentation for _empty_/Alpha.apply().
         |""".stripMargin
    )

  @Test def `class` =
    check(
      """|
         |/**
         |  * Doc about class
         |  *
         |  */
         |class Alpha(x: Int)
         |
         |object Alpha {
         |  def apply(x: Int) = x + 1
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def apply(x: Int): Int
         |```
         |Found documentation for _empty_/Alpha.apply().
         |""".stripMargin
    )

  @Test def `universal-apply` =
    check(
      """|
         |/**
         |  * Doc about class
         |  *
         |  */
         |class Alpha(x: Int)
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|```scala
         |def this(x: Int): Alpha
         |```
         |Found documentation for _empty_/Alpha#
         |""".stripMargin
    )

  @Test def `i7093` =
    check(
      """|object O:
         |  /** Ooopsie daisy */
         |  val computeLogicOwners: Unit =
         |    /** This is a comment */
         |    <<def logi@@cOwners = ???>>
         |    ???
         |""".stripMargin,
      """def logicOwners: Nothing""".hover.stripMargin
    )
