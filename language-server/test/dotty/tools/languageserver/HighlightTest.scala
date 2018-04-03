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

}
