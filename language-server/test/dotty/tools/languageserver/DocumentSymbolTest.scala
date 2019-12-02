package dotty.tools.languageserver

import org.junit.Test
import org.eclipse.lsp4j.SymbolKind

import dotty.tools.languageserver.util.Code._


class DocumentSymbolTest {

  @Test def withErroneousTree: Unit =
    code"class ${m1}Foo$m2 { def }"
      .withSource.documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class))

  @Test def documentSymbol0: Unit =
    code"class ${m1}Foo$m2".withSource.documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class))

  @Test def documentSymbol1: Unit =
    code"class ${m1}Foo$m2; class ${m3}Bar$m4".withSource
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class), (m3 to m4).symInfo("Bar", SymbolKind.Class))

  @Test def documentSymbol3: Unit = {
    withSources(
      code"class ${m1}Foo$m2",
      code"class ${m3}Bar$m4"
    ) .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class))
      .documentSymbol(m3, (m3 to m4).symInfo("Bar", SymbolKind.Class))
  }

  @Test def documentSymbolShowModule: Unit = {
    code"""object ${m1}Foo${m2}""".withSource
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Module))
  }

  @Test def documentSymbolShowClassAndCompanion: Unit = {
    code"""object ${m1}Foo${m2}
          |class ${m3}Foo${m4}""".withSource
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Module),
                          (m3 to m4).symInfo("Foo", SymbolKind.Class))
  }

  @Test def documentSymbolSynthetic: Unit = {
    code"""case class ${m1}Foo${m2}(${m3}x${m4}: Int)""".withSource
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", SymbolKind.Class),
                          (m3 to m4).symInfo("x", SymbolKind.Field, "Foo"))
  }
}
