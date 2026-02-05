package dotty.tools.pc.tests.info

import java.nio.file.Paths

import scala.annotation.nowarn
import scala.language.unsafeNulls
import scala.meta.internal.jdk.CollectionConverters.*
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.PcSymbolInformation
import scala.meta.pc.PcSymbolKind
import scala.meta.pc.PcSymbolProperty

import dotty.tools.pc.base.BasePCSuite

import org.junit.Test

class InfoSuite extends BasePCSuite {

  def getInfo(symbol: String): PcSymbolInformation =
    val result = presentationCompiler.info(symbol).get()
    assertEquals(true, result.isPresent(), s"no info returned for symbol $symbol")
    assertNoDiff(result.get().symbol(), symbol)
    result.get()

  @Test def `list` =
    val info = getInfo("scala/collection/immutable/List#")
    assertEquals(true, info.properties().contains(PcSymbolProperty.ABSTRACT), s"class List should be abstract")
    assertEquals(
      true,
      info.parents().contains("scala/collection/immutable/LinearSeq#"),
      "class List should extend LinearSeq"
    )

  @Test def `empty-list-constructor` =
    val info = getInfo("scala/collection/immutable/List.empty().")
    assertNoDiff(info.classOwner(), "scala/collection/immutable/List.")
    assertEquals(info.kind(), PcSymbolKind.METHOD, "List.empty() should be a method")

  @Test def `assert` =
    val info = getInfo("scala/Predef.assert().")
    assertEquals(info.kind(), PcSymbolKind.METHOD, "assert() should be a method")
    assertNoDiff(info.classOwner(), "scala/Predef.")
    assertEquals(
      info.alternativeSymbols().asScala.mkString("\n"),
      "scala/Predef.assert(+1).",
      "there should be a single alternative symbol to assert()"
    )

  @Test def `flatMap` =
    val info = getInfo("scala/collection/immutable/List#flatMap().")
    assertEquals(info.kind(), PcSymbolKind.METHOD, "List.flatMap() should be a method")
    assertNoDiff(info.classOwner(), "scala/collection/immutable/List#")
    assertNoDiff(
      info.overriddenSymbols().asScala.mkString("\n"),
      """|scala/collection/StrictOptimizedIterableOps#flatMap().
         |scala/collection/IterableOps#flatMap().
         |scala/collection/IterableOnceOps#flatMap().
         |""".stripMargin
    )

  @Test def i7251 =
    withSource(
      """|package a
         |sealed trait TA:
         |  type SomeType
         |trait TB extends TA:
         |  type SomeType = Int
         |""".stripMargin
    )
    val info = presentationCompiler.info("a/TA#SomeType#").get()
    assertNoDiff(info.get().symbol(), "a/TA#SomeType#")

  @Test def memberDefsAnnotations =
    def assertMemberDefsAnnotations(symbol: String, expected: String) =
      val info = presentationCompiler.info(symbol).get()
      assertNoDiff(info.get().memberDefsAnnotations().asScala.mkString("\n"), expected, Some(symbol))
    withSource(
      """|package a
         |import scala.annotation.nowarn
         |sealed trait TA:
         | @nowarn
         | def aaa = 1
         |
         |object O:
         | @nowarn
         | def aaa = 1
         |
         |class D:
         | @nowarn
         | def bbb = 1
         |""".stripMargin
    )
    assertMemberDefsAnnotations("a/TA#", "scala.annotation.nowarn")
    assertMemberDefsAnnotations("a/O.", "scala.annotation.nowarn")
    assertMemberDefsAnnotations("a/D#", "scala.annotation.nowarn")
    assertMemberDefsAnnotations("a/D#bbb().", "")

  @Test def `self-type` =
    withSource(
      """|package a
         |trait A { def aa: Unit }
         |trait B {
         | this : A =>
         |  override def aa: Unit = ()
         |}
         |""".stripMargin
    )
    val info = getInfo("a/B#")
    assertNoDiff(
      info.parents().asScala.mkString("\n"),
      """|a/A#
         |java/lang/Object#
         |""".stripMargin
    )

  @Test def `self-type-1` =
    withSource(
      """|package a
         |trait A {
         |  def aa(i: Int): String = ""
         |  def aa: Unit
         |}
         |trait B {
         | this : A =>
         |  override def aa: Unit = ()
         |}
         |""".stripMargin
    )
    val info = getInfo("a/B#")
    assertNoDiff(
      info.parents().asScala.mkString("\n"),
      """|a/A#
         |java/lang/Object#
         |""".stripMargin
    )

  @Test def `self-type-with` =
    withSource(
      """|package a
         |trait C
         |trait A { def aa: Unit }
         |trait B {
         | this : A with C =>
         |  override def aa: Unit = ()
         |}
         |""".stripMargin
    )
    val info = getInfo("a/B#")
    assertNoDiff(
      info.parents().asScala.mkString("\n"),
      """|a/A#
         |a/C#
         |java/lang/Object#
         |""".stripMargin
    )

  @Test def `self-type-in-lib` =
    val info = getInfo("scala/collection/generic/DefaultSerializable#")
    assertNoDiff(
      info.parents().asScala.mkString("\n"),
      """|scala/collection/Iterable#
         |java/lang/Object#
         |java/io/Serializable#
         |""".stripMargin
    )

  // hacky way to add a source file to the presentation compiler sources
  private def withSource(code: String) =
    val filename = "Hover.scala"
    val pcParams = CompilerOffsetParams(Paths.get(filename).toUri(), code, 0)
    presentationCompiler.hover(pcParams).get()
}
