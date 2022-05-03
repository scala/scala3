package dotty.tools.languageserver

import org.junit.Test
import org.eclipse.lsp4j.SymbolKind

import dotty.tools.languageserver.util.Code._


class DocumentSymbolTest {

  @Test def withErroneousTree: Unit =
    code"${m1}class Foo { def }$m2"
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class))

  @Test def documentSymbol0: Unit =
    code"${m1}class Foo$m2"
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class))

  @Test def documentSymbol1: Unit =
    code"${m1}class Foo$m2; ${m3}class Bar$m4"
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class), (m3 to m4).symInfo("Bar", SymbolKind.Class))

  @Test def documentSymbol3: Unit = {
    withSources(
      code"${m1}class Foo$m2",
      code"${m3}class Bar$m4"
    ) .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class))
      .documentSymbol(m3, (m3 to m4).symInfo("Bar", SymbolKind.Class))
  }

  @Test def documentSymbolShowModule: Unit = {
    code"""${m1}object Foo${m2}"""
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Module))
  }

  @Test def documentSymbolShowClassAndCompanion: Unit = {
    code"""${m1}object Foo${m2}
          |${m3}class Foo${m4}"""
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Module),
                          (m3 to m4).symInfo("Foo", SymbolKind.Class))
  }

  @Test def documentSymbolSynthetic: Unit = {
    code"""${m1}case class Foo(${m3}x: Int${m4})${m2}"""
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class),
                          (m3 to m4).symInfo("x", SymbolKind.Field, "Foo"))
  }

  @Test def documentSymbolEnumADT: Unit = {
    code"""${m1}enum Option[${m3}+T${m4}] {
      ${m5}case Some(${m7}x: T${m8})${m6}
      ${m9}case None${m10}
    }${m2}"""
      .documentSymbol(m1, (m1 to m2).symInfo("Option", SymbolKind.Enum),
                          (m3 to m4).symInfo("T", SymbolKind.TypeParameter, "Option"),
                          (m5 to m6).symInfo("Some", SymbolKind.EnumMember, "Option"),
                          (m7 to m8).symInfo("x", SymbolKind.Field, "Some"),
                          (m9 to m10).symInfo("None", SymbolKind.EnumMember, "Option"))
  }

  @Test def documentSymbolEnum: Unit = {
    code"""${m1}enum Color(${m3}val rgb: Int${m4}) {
      ${m5}case Red   extends Color(0xFF0000)${m6}
      ${m7}case Green extends Color(0x00FF00)${m8}
      ${m9}case Blue  extends Color(0x0000FF)${m10}
    }${m2}"""
      .documentSymbol(m1, (m1 to m2).symInfo("Color", SymbolKind.Enum),
                          (m3 to m4).symInfo("rgb", SymbolKind.Field, "Color"),
                          (m5 to m6).symInfo("Red", SymbolKind.EnumMember, "Color"),
                          (m7 to m8).symInfo("Green", SymbolKind.EnumMember, "Color"),
                          (m9 to m10).symInfo("Blue", SymbolKind.EnumMember, "Color"))
  }

  @Test def documentSymbolTopLevelDef: Unit =
    code"${m1}def foo(): Unit = { }${m2}"
      .documentSymbol(m1, (m1 to m2).symInfo("foo", SymbolKind.Method))

  @Test def documentSymbolTrait: Unit =
    code"${m1}trait Foo(${m3}val x: Int${m4})${m2}"
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Interface),
                          (m3 to m4).symInfo("x", SymbolKind.Field, "Foo"))

  @Test def documentSymbolLocalDef: Unit =
    code"""${m1}def foo(): Unit = {
      ${m3}def bar(): Unit = { }${m4}
      ${m5}val x: Int = 0${m6}
    }${m2}"""
      .documentSymbol(m1, (m1 to m2).symInfo("foo", SymbolKind.Method),
                          (m3 to m4).symInfo("bar", SymbolKind.Method, "foo"),
                          (m5 to m6).symInfo("x", SymbolKind.Field, "foo"))

  @Test def documentSymbolTypeFields: Unit =
    code"""${m1}class Foo {
      ${m3}type T${m4}
    }${m2}""".documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class),
                                 (m3 to m4).symInfo("T", SymbolKind.TypeParameter, "Foo"))

}
