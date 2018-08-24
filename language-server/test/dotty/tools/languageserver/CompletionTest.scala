package dotty.tools.languageserver

import org.junit.Test
import org.eclipse.lsp4j.CompletionItemKind

import dotty.tools.languageserver.util.Code._

class CompletionTest {

  @Test def completion0: Unit = {
    code"class Foo { val xyz: Int = 0; def y: Int = xy$m1 }".withSource
      .completion(m1, Set(("xyz", CompletionItemKind.Field, "Int")))
  }

  @Test def completionWithImplicitConversion: Unit = {
    withSources(
      code"object Foo { implicit class WithBaz(bar: Bar) { def baz = 0 } }",
      code"class Bar",
      code"object Main { import Foo._; val bar: Bar = new Bar; bar.b${m1} }"
    ) .completion(m1, Set(("baz", CompletionItemKind.Method, "=> Int")))
  }
}
