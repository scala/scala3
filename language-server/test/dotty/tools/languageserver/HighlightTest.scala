package dotty.tools.languageserver

import org.junit.Test
import dotty.tools.languageserver.util.Code._
import org.eclipse.lsp4j.DocumentHighlightKind

class HighlightTest {

  @Test def valHighlight0: Unit = {
    val xDef = (m1 to m2).withCode("x")
    code"class X { val $xDef = 9 }".withSource
      .highlight(xDef.range, (xDef.range, DocumentHighlightKind.Read))
  }

  @Test def valHighlight1: Unit = {
    val xDef = (m1 to m2).withCode("x")
    val xRef = (m3 to m4).withCode("x")
    code"class X { val $xDef = 9; $xRef}".withSource
      .highlight(xRef.range, (xDef.range, DocumentHighlightKind.Read), (xRef.range, DocumentHighlightKind.Read))
  }

  @Test def highlightClass(): Unit = {
    code"""class ${m1}Foo${m2} { new ${m3}Foo${m4} }""".withSource
      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
  }

}
