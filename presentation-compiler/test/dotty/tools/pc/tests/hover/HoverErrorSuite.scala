package dotty.tools.pc.tests.hover

import org.junit.Test
import dotty.tools.pc.base.BaseHoverSuite

class HoverErrorSuite extends BaseHoverSuite:
  override def requiresJdkSources: Boolean = true

  @Test def `no-type` =
    check(
      """|object Main extends App{
         |  def hello(<<a@@aa>>) : Int = ""
         |}
         |""".stripMargin,
      ""
    )

  @Test def `error` =
    check(
      """|final case class Dependency(
         |    org: String,
         |    name: Option[String],
         |    version: Option[String]
         |)
         |
         |object Dependency {
         |  def <<ap@@ply>>(org: String) = Dependency(org, None, None)
         |}
         |""".stripMargin,
      "".stripMargin
    )
