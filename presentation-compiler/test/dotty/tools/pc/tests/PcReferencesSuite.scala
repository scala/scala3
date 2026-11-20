package dotty.tools.pc.tests

import java.net.URI

import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.pc.PcReferencesRequest

import dotty.tools.pc.base.BasePCSuite
import dotty.tools.pc.utils.RangeReplace

import org.eclipse.lsp4j.jsonrpc.messages.Either as JEither
import org.junit.Test

class PcReferencesSuite extends BasePCSuite with RangeReplace {
  def check(
      original: String
  ): Unit =
    val edit = original.replaceAll("(<<|>>)", "")
    val expected = original.replaceAll("@@", "")
    val base = original.replaceAll("(<<|>>|@@)", "")

    val (code, offset) = params(edit, "Highlight.scala")
    val ranges = presentationCompiler
      .references(
        PcReferencesRequest(
          CompilerVirtualFileParams(
            URI.create("file:/Highlight.scala"),
            code,
            EmptyCancelToken
          ),
          includeDefinition = false,
          offsetOrSymbol = JEither.forLeft(offset)
        )
      )
      .get()
      .asScala
      .flatMap(_.locations().asScala.map(_.getRange()))
      .toList

    assertEquals(
      renderRangesAsString(base, ranges),
      expected,
      "references should match"
    )

  @Test def `implicit-args` =
    check(
      """|package example
         |
         |class Bar(i: Int)
         |
         |object Hello {
         |  def m(i: Int)(implicit b: Bar) = ???
         |  val foo = {
         |    implicit val ba@@rr: Bar = new Bar(1)
         |    m(3)<<>>
         |  }
         |}
         |""".stripMargin
    )

  @Test def `implicit-args-2` =
    check(
      """|package example
        |
        |class Bar(i: Int)
        |class Foo(implicit b: Bar)
        |
        |object Hello {
        |  implicit val ba@@rr: Bar = new Bar(1)
        |  val foo = new Foo<<>>
        |}
        |""".stripMargin
    )

  @Test def `case-class` =
    check(
      """|case class Ma@@in(i: Int)
         |""".stripMargin
    )

  @Test def `case-class-with-implicit` =
    check(
      """"|case class A()(implicit val fo@@o: Int)
          |""".stripMargin
    )

  @Test def `named-tuple-field` =
    check(
      """|object Main:
         |  val x: (name: String, age: Int) = ???
         |  val other: (name: String, age: Int) = ???
         |  val y = x.<<name>>
         |  val z = x.<<na@@me>>
         |  val xAge = x.age
         |  val otherName = other.<<name>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-nested` =
    check(
      """|object Main:
         |  val x: (inner: (name: String, age: Int)) = ???
         |  val y: (inner: (name: String, age: Int)) = ???
         |  val a = x.inner.<<na@@me>>
         |  val b = x.inner.<<name>>
         |  val c = y.inner.<<name>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-method-qualifier` =
    check(
      """|object Main:
         |  def person(): (name: String, age: Int) = ???
         |  val a = person().<<na@@me>>
         |  val b = person().<<name>>
         |  val c = person().age
         |""".stripMargin
    )

  @Test def `named-tuple-field-scopes` =
    check(
      """|object Main:
         |  val x: (name: String, age: Int) = ???
         |  val y = x.name
         |  def g(p: (name: String)) = p.<<na@@me>>
         |  def h(p: (name: String)) = p.<<name>>
         |""".stripMargin
    )

  @Test def `named-tuple-field-different-types` =
    check(
      """|object Main:
         |  val people: List[(name: String, age: Int)] = ???
         |  val cars: List[(name: String, hp: Int)] = ???
         |  val a = people.head.<<na@@me>>
         |  val b = people.head.<<name>>
         |  val c = cars.head.name
         |""".stripMargin
    )

  @Test def `named-tuple-field-from-case-class` =
    check(
      """|object Main:
         |  case class Person(name: String, age: Int)
         |  val p: NamedTuple.From[Person] = ???
         |  val q: NamedTuple.From[Person] = ???
         |  val a = p.<<a@@ge>>
         |  val b = q.<<age>>
         |  val c = p.name
         |""".stripMargin
    )
}
