package dotty.tools.pc.tests.edit

import java.net.URI
import java.util.concurrent.ExecutionException

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.internal.pc.CodeActionErrorMessages

import dotty.tools.pc.base.BaseCodeActionSuite
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j as l
import org.junit.Test

class ConvertToNamedArgumentsSuite extends BaseCodeActionSuite:

  @Test def `scala-std-lib` =
    checkEdit(
      """|object A{
         |  val a = <<scala.math.max(1, 2)>>
         |}""".stripMargin,
      List(0, 1),
      """|object A{
         |  val a = scala.math.max(x = 1, y = 2)
         |}""".stripMargin
    )

  @Test def `backticked-name` =
    checkEdit(
      """|object A{
         |  final case class Foo(`type`: Int, arg: String)
         |  val a = <<Foo(1, "a")>>
         |}""".stripMargin,
      List(0, 1),
      """|object A{
         |  final case class Foo(`type`: Int, arg: String)
         |  val a = Foo(`type` = 1, arg = "a")
         |}""".stripMargin
    )

  @Test def `backticked-name-method` =
    checkEdit(
      """|object A{
         |  def foo(`type`: Int, arg: String) = "a"
         |  val a = <<foo(1, "a")>>
         |}""".stripMargin,
      List(0, 1),
      """|object A{
         |  def foo(`type`: Int, arg: String) = "a"
         |  val a = foo(`type` = 1, arg = "a")
         |}""".stripMargin
    )

  @Test def `new-apply` =
    checkEdit(
      """|object Something {
         |  class Foo(param1: Int, param2: Int)
         |  val a = <<new Foo(1, param2 = 2)>>
         |}""".stripMargin,
      List(0),
      """|object Something {
         |  class Foo(param1: Int, param2: Int)
         |  val a = new Foo(param1 = 1, param2 = 2)
         |}""".stripMargin
    )

  @Test def `new-apply-multiple` =
    checkEdit(
      """|object Something {
         |  class Foo(param1: Int, param2: Int)(param3: Int)
         |  val a = <<new Foo(1, param2 = 2)(3)>>
         |}""".stripMargin,
      List(0, 2),
      """|object Something {
         |  class Foo(param1: Int, param2: Int)(param3: Int)
         |  val a = new Foo(param1 = 1, param2 = 2)(param3 = 3)
         |}""".stripMargin
    )

  @Test def `java-object` =
    checkError(
      """|object A{
         |  val a = <<new java.util.Vector(3)>>
         |}
         |""".stripMargin,
      List(0, 1),
      CodeActionErrorMessages.ConvertToNamedArguments.IsJavaObject
    )

  def checkError(
      original: String,
      argIndices: List[Int],
      expectedErrorMsg: String
  ): Unit =
    try
      val edits = convertToNamedArgs(original, argIndices)
      val (code, _, _) = params(original)
      val obtained = TextEdits.applyEdits(code, edits)
      fail(s"No error. Result: \n $obtained")
    catch
      case e: ExecutionException =>
        e.getCause() match
          case cause => assertNoDiff(expectedErrorMsg, cause.getMessage)

  def checkEdit(
      original: String,
      argIndices: List[Int],
      expected: String
  ): Unit =
    val edits = convertToNamedArgs(original, argIndices)
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)
    assertNoDiff(expected, obtained)

  def convertToNamedArgs(
      original: String,
      argIndices: List[Int],
      filename: String = "file:/A.scala"
  ): List[l.TextEdit] =
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .convertToNamedArguments(
        CompilerOffsetParams(URI.create(filename), code, offset, cancelToken),
        argIndices.map(Integer.valueOf).asJava
      )
      .get()
    result.asScala.toList
