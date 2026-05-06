package dotty.tools.pc.tests.edit

import java.net.URI
import java.util.Optional

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.CodeActionId
import scala.meta.pc.DisplayableException

import dotty.tools.pc.PcConvertToNamedLambdaParameters
import dotty.tools.pc.base.BaseCodeActionSuite
import dotty.tools.pc.utils.TextEdits

import org.eclipse.lsp4j as l
import org.junit.{Ignore, Test}

class ConvertToNamedLambdaParametersSuite extends BaseCodeActionSuite:

  @Test def `Int => Int function in map` =
    checkEdit(
      """|object A{
      |  val a = List(1, 2).map(<<_>> + 1)
      |}""".stripMargin,
      """|object A{
      |  val a = List(1, 2).map(i => i + 1)
      |}""".stripMargin
    )

  @Test def `Int => Int function in map with another wildcard lambda` =
    checkEdit(
      """|object A{
      |  val a = List(1, 2).map(<<_>> + 1).map(_ + 1)
      |}""".stripMargin,
      """|object A{
      |  val a = List(1, 2).map(i => i + 1).map(_ + 1)
      |}""".stripMargin
    )

  @Test def `String => String function in map` =
    checkEdit(
      """|object A{
      |  val a = List("a", "b").map(<<_>> + "c")
      |}""".stripMargin,
      """|object A{
      |  val a = List("a", "b").map(s => s + "c")
      |}""".stripMargin
    )

  @Test def `Person => Person function to custom method` =
    checkEdit(
      """|object A{
      |  case class Person(name: String, age: Int)
      |  val bob = Person("Bob", 30)
      |  def m[A](f: Person => A): A = f(bob)
      |  m(_<<.>>name)
      |}
      |""".stripMargin,
      """|object A{
      |  case class Person(name: String, age: Int)
      |  val bob = Person("Bob", 30)
      |  def m[A](f: Person => A): A = f(bob)
      |  m(p => p.name)
      |}
      |""".stripMargin
    )

  @Test def `(String, Int) => Int function in map with multiple underscores` =
    checkEdit(
      """|object A{
      |  val a = List(("a", 1), ("b", 2)).map(<<_>> + _)
      |}""".stripMargin,
      """|object A{
      |  val a = List(("a", 1), ("b", 2)).map((s, i) => s + i)
      |}""".stripMargin
    )

  @Test def `Int => Int function in map with multiple underscores` =
    checkEdit(
      """|object A{
      |  val a = List(1, 2).map(x => x -> (x + 1)).map(<<_>> + _)
      |}""".stripMargin,
      """|object A{
      |  val a = List(1, 2).map(x => x -> (x + 1)).map((i, i1) => i + i1)
      |}""".stripMargin
    )

  @Test def `Int => Float function in nested lambda 1` =
    checkEdit(
      """|object A{
      |  val a = List(1, 2).flatMap(List(_).flatMap(v => List(v, v + 1).map(<<_>>.toFloat)))
      |}""".stripMargin,
      """|object A{
      |  val a = List(1, 2).flatMap(List(_).flatMap(v => List(v, v + 1).map(i => i.toFloat)))
      |}""".stripMargin
    )

  @Test def `Int => Float function in nested lambda 2` =
    checkEdit(
      """|object A{
      |  val a = List(1, 2).flatMap(List(<<_>>).flatMap(v => List(v, v + 1).map(_.toFloat)))
      |}""".stripMargin,
      """|object A{
      |  val a = List(1, 2).flatMap(i => List(i).flatMap(v => List(v, v + 1).map(_.toFloat)))
      |}""".stripMargin
    )

  @Test def `Int => Float function in nested lambda with shadowing` =
    checkEdit(
      """|object A{
      |  val a = List(1, 2).flatMap(List(<<_>>).flatMap(i => List(i, i + 1).map(_.toFloat)))
      |}""".stripMargin,
      """|object A{
      |  val a = List(1, 2).flatMap(i1 => List(i1).flatMap(i => List(i, i + 1).map(_.toFloat)))
      |}""".stripMargin
    )

  @Test def `(String, String, String, String, String, String, String) => String function in map` =
    checkEdit(
      """|object A{
      |  val a = List(
      |    ("a", "b", "c", "d", "e", "f", "g"),
      |    ("h", "i", "j", "k", "l", "m", "n")
      |  ).map(_<< >>+ _ + _ + _ + _ + _ + _)
      |}""".stripMargin,
      """|object A{
      |  val a = List(
      |    ("a", "b", "c", "d", "e", "f", "g"),
      |    ("h", "i", "j", "k", "l", "m", "n")
      |  ).map((s, s1, s2, s3, s4, s5, s6) => s + s1 + s2 + s3 + s4 + s5 + s6)
      |}""".stripMargin
    )

  @Test def `Long => Long with match and wildcard pattern` =
    checkEdit(
      """|object A{
      |  val a = List(1L, 2L).map(_ match {
      |    case 1L => 1L
      |    case _ => <<2L>>
      |  })
      |}""".stripMargin,
      """|object A{
      |  val a = List(1L, 2L).map(l => l match {
      |    case 1L => 1L
      |    case _ => 2L
      |  })
      |}""".stripMargin
    )

  @Ignore
  @Test def `Int => Int eta-expansion in map` =
    checkEdit(
      """|object A{
        |  def f(x: Int): Int = x + 1
        |  val a = List(1, 2).map(<<f>>)
        |}""".stripMargin,
      """|object A{
        |  def f(x: Int): Int = x + 1
        |  val a = List(1, 2).map(i => f(i))
        |}""".stripMargin
    )

  def checkEdit(
      original: String,
      expected: String,
      compat: Map[String, String] = Map.empty
  ): Unit =
    val edits = convertToNamedLambdaParameters(original)
    val (code, _, _) = params(original)
    val obtained = TextEdits.applyEdits(code, edits)
    assertNoDiff(expected, obtained)

  def convertToNamedLambdaParameters(
      original: String,
      filename: String = "file:/A.scala"
  ): List[l.TextEdit] =
    val (code, _, offset) = params(original)
    val result = presentationCompiler
      .codeAction(
        CompilerOffsetParams(URI.create(filename), code, offset, cancelToken),
        PcConvertToNamedLambdaParameters.codeActionId,
        Optional.empty()
      )
      .get()
    result.asScala.toList
