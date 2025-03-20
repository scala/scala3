package dotty.tools.pc.tests.edit

import java.net.URI
import java.util.Optional

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.CodeActionId

import org.eclipse.lsp4j.TextEdit
import dotty.tools.pc.base.BaseCodeActionSuite
import main.dotty.tools.pc.PcConvertToEnum
import dotty.tools.pc.utils.TextEdits

import org.junit.Test

class ConvertToEnumSuite extends BaseCodeActionSuite:

  @Test def basic =
    checkEdit(
      """|sealed trait <<C>>ow
        |object Cow:
        |  class HolsteinFriesian extends Cow
        |  class Highland extends Cow
        |  class BrownSwiss extends Cow
        |""".stripMargin,
      """|enum Cow:
        |  case HolsteinFriesian, Highland, BrownSwiss
        |""".stripMargin
    )

  @Test def `basic-with-params` =
    checkEdit(
      """|sealed class <<C>>ow[T](val i: Int, j: Int)
        |object Cow:
        |  class HolsteinFriesian extends Cow[1](1, 1)
        |  class Highland extends Cow[2](2, 2)
        |  class BrownSwiss extends Cow[3](3, 3)
        |""".stripMargin,
      """|enum Cow[T](val i: Int, j: Int):
        |  case HolsteinFriesian extends Cow[1](1, 1)
        |  case Highland extends Cow[2](2, 2)
        |  case BrownSwiss extends Cow[3](3, 3)
        |""".stripMargin
    )

  @Test def `class-with-body` =
    checkEdit(
      """|trait Spotted
        |
        |sealed trait <<C>>ow:
        |  def moo = "Mooo!"
        |
        |object Cow:
        |  def of(name: String) = HolsteinFriesian(name)
        |  case class HolsteinFriesian(name: String) extends Cow, Spotted
        |  class Highland extends Cow
        |  class BrownSwiss extends Cow
        |""".stripMargin,
      """|trait Spotted
        |
        |enum Cow:
        |  def moo = "Mooo!"
        |  case Highland, BrownSwiss
        |  case HolsteinFriesian(name: String) extends Cow, Spotted
        |
        |object Cow:
        |  def of(name: String) = HolsteinFriesian(name)
        |""".stripMargin
    )

  @Test def `with-indentation` =
    checkEdit(
      """|object O {
        |  sealed class <<C>>ow {
        |    def moo = "Mooo!"
        |    def mooooo = "Mooooooo!"
        |  }
        |  object Cow {
        |    case class HolsteinFriesian(name: String) extends Cow
        |    class Highland extends Cow
        |    class BrownSwiss extends Cow
        |  }
        |}
        |""".stripMargin,
      """|object O {
        |  enum Cow {
        |    def moo = "Mooo!"
        |    def mooooo = "Mooooooo!"
        |    case Highland, BrownSwiss
        |    case HolsteinFriesian(name: String) extends Cow
        |  }
        |}
        |""".stripMargin
    )

  @Test def `case-objects` =
    checkEdit(
      """|sealed trait <<C>>ow
        |case object HolsteinFriesian extends Cow
        |case object Highland extends Cow
        |case object BrownSwiss extends Cow
        |""".stripMargin,
      """|enum Cow:
        |  case HolsteinFriesian, Highland, BrownSwiss
        |export Cow.*
        |""".stripMargin
    )

  @Test def `no-companion-object` =
    checkEdit(
      """|sealed trait <<C>>ow
        |class HolsteinFriesian extends Cow
        |class Highland extends Cow
        |class BrownSwiss extends Cow
        |""".stripMargin,
      """|enum Cow:
        |  case HolsteinFriesian, Highland, BrownSwiss
        |export Cow.*
        |""".stripMargin
    )

  def checkError(
      original: String,
      expectedError: String
  ): Unit =
    Try(getConversionToEnum(original)) match
      case Failure(exception: Throwable) =>
        assertNoDiff(
          exception.getCause().getMessage().replaceAll("\\[.*\\]", ""),
          expectedError
        )
      case Success(_) =>
        fail("Expected an error but got a result")

  def checkEdit(
      original: String,
      expected: String,
  ): Unit =
    val edits = getConversionToEnum(original)
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)
    assertNoDiff(obtained, expected)

  def getConversionToEnum(
      original: String,
      filename: String = "file:/A.scala"
  ): List[TextEdit] = {
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .codeAction[Boolean](
        CompilerOffsetParams(URI.create(filename), code, offset, cancelToken),
        PcConvertToEnum.codeActionId,
        Optional.empty()
      )
      .get()
    result.asScala.toList
  }
