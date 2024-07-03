package dotty.tools.pc.tests.info

import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.pc.PcSymbolKind
import scala.meta.pc.PcSymbolProperty

import scala.meta.pc.PcSymbolInformation
import dotty.tools.pc.base.BasePCSuite
import scala.language.unsafeNulls
import org.junit.Test

class InfoSuite extends BasePCSuite {

  def getInfo(symbol: String): PcSymbolInformation = {
    val result = presentationCompiler.info(symbol).get()
    assertEquals(true, result.isPresent(), s"no info returned for symbol $symbol")
    assertNoDiff(result.get().symbol(), symbol)
    result.get()
  }

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
}
