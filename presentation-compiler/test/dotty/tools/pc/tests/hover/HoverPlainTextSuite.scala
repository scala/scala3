package dotty.tools.pc.tests.hover

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.ContentType
import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolDocumentation

import dotty.tools.pc.base.BaseHoverSuite
import dotty.tools.pc.utils.MockEntries

import org.junit.Test

class HoverPlainTextSuite extends BaseHoverSuite:

  override def config: PresentationCompilerConfigImpl =
    super.config.copy(
      snippetAutoIndent = false,
      hoverContentType = ContentType.PLAINTEXT
    )

  override protected def mockEntries: MockEntries = new MockEntries:
    override def documentations: Set[SymbolDocumentation] = Set(
      ScalaMockDocumentation("java/lang/String#substring().", "substring", List(), List(MockParam("beginIndex"))),
      ScalaMockDocumentation("java/util/Collections#emptyList().", "emptyList"),
      ScalaMockDocumentation("_empty_/Alpha.apply().", "apply", List(), List(MockParam("x"))),
      ScalaMockDocumentation("_empty_/Alpha#", "init", List(), List(MockParam("x"))),
      ScalaMockDocumentation("scala/collection/LinearSeqOps#headOption().", "headOption"),
      ScalaMockDocumentation("scala/Option#fold().", "fold", List(MockParam("B")))
    )

  @Test def `basic-plaintext` =
    check(
      """|
         |/**
         |  * Some docstring
         |  */
         |case class Alpha(x: Int) {
         |}
         |
         |object Main {
         |  val x = <<Alp@@ha(2)>>
         |}
         |""".stripMargin,
      """|def apply(x: Int): Alpha
         |
         |Found documentation for _empty_/Alpha.apply().
         |
         |""".stripMargin
    )

  @Test def `head-plaintext` =
    check(
      """|object a {
         |  <<List(1).he@@adOption>>
         |}
         |""".stripMargin,
      """|override def headOption: Option[Int]
         |
         |Found documentation for scala/collection/LinearSeqOps#headOption().
         |""".stripMargin
    )

  @Test def `trait-plaintext` =
    check(
      """|trait XX
         |object Main extends <<X@@X>>{}
         |""".stripMargin,
      "trait XX: XX"
    )

  @Test def `function-chain4-plaintext` =
    check(
      """
        |trait Consumer {
        |  def subConsumer[T](i: T): T
        |  def consume(value: Int)(n: Int): Unit
        |}
        |
        |object O {
        |  val consumer: Consumer = ???
        |  List(1).foreach(<<consumer.su@@bConsumer(consumer)>>.consume(1))
        |}
        |""".stripMargin,
      """|Expression type:
         |Consumer
         |
         |Symbol signature:
         |def subConsumer[T](i: T): T
         |""".stripMargin
    )
