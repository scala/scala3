package dotty.tools.pc.tests.completion

import java.net.URI
import java.nio.file.Path

import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.pc.CancelToken
import scala.meta.pc.VirtualFileParams

import dotty.tools.pc.RawScalaPresentationCompiler
import dotty.tools.pc.base.BaseCompletionSuite
import dotty.tools.pc.base.TestResources
import dotty.tools.pc.tests.buildinfo.BuildInfo
import dotty.tools.pc.utils.PcAssertions

import org.junit.Test

class ScalaJSCompletionsSuite extends BaseCompletionSuite {

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] = List("-scalajs")

  override protected def additionalClasspath: Seq[Path] = BuildInfo.ideTestsScalaJSClasspath.map(_.toPath)

  @Test def plainScalaJS =
    check(
      """
        |object ChatApp:
        |  private def connectWebSocket() =
        |    scala.scalajs.js.@@
        |    ???
        |""".stripMargin,
      """
        |WrappedArray[A](elems: A*): WrappedArray[A]
        |""".stripMargin,
      topLines = Some(4),
      filter = _.contains("WrappedArray")
    )

  @Test def scalajsDom =
    check(
      """
        |
        |import org.scalajs.dom
        |import org.scalajs.dom.WebSocket
        |
        |object ChatApp:
        |  private def connectWebSocket() =
        |    val newws = WebSocket(???)
        |    dom.window.onbeforeunload@@ = _ => newws.close()
        |""".stripMargin,
      """
        |onbeforeunload: scala.scalajs.js.Function1[BeforeUnloadEvent, ?]
        |""".stripMargin
    )

  @Test def union =
    check(
      """
        |
        |import org.scalajs.dom
        |import org.scalajs.dom.WebSocket
        |
        |object ChatApp:
        |  val foo: String | Unit = "blah"
        |  foo@@
        |""".stripMargin,
      """
        |foo: String | Unit
        |""".stripMargin
    )
}
