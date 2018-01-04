package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class DocumentSymbolTest {

  @Test def documentSymbol0: Unit =
    code"class ${m1}Foo$m2".withSource.documentSymbol(m1, (m1 to m2).symInfo("Foo", "Class"))

  @Test def documentSymbol1: Unit =
    code"class ${m1}Foo$m2; class ${m3}Bar$m4".withSource
      .documentSymbol(m1, (m1 to m2).symInfo("Foo", "Class"), (m3 to m4).symInfo("Bar", "Class"))

  @Test def documentSymbol3: Unit = {
    withSources(
      code"class ${m1}Foo$m2",
      code"class ${m3}Bar$m4"
    ) .documentSymbol(m1, (m1 to m2).symInfo("Foo", "Class"))
      .documentSymbol(m3, (m3 to m4).symInfo("Bar", "Class"))
  }
}
