package dotty.tools.languageserver

import org.junit.Test
import org.eclipse.lsp4j.SymbolKind

import dotty.tools.languageserver.util.Code._

class SymbolTest {

 @Test def symbol0: Unit = {
   val Foo = (m1 to m2).withCode("Foo")
   withSources(code"class $Foo").symbol("Foo", Foo.range.symInfo("Foo", SymbolKind.Class))
 }

  @Test def symbol1: Unit = {
    val Foo = (m1 to m2).withCode("Foo")
    val Bar = (m3 to m4).withCode("Bar")
    val fooFoo = (m5 to m6).withCode("Foo")
    withSources(
      code"class $Foo",
      code"class $Bar",
      code"""package foo
            |class $fooFoo {
            |  class ${m7}Bar$m8
            |}
          """
    ) .symbol("Foo", Foo.range.symInfo("Foo", SymbolKind.Class), fooFoo.range.symInfo("Foo", SymbolKind.Class, "foo"))
      .symbol("Bar", Bar.range.symInfo("Bar", SymbolKind.Class), (m7 to m8).symInfo("Bar", SymbolKind.Class, "Foo"))
  }

  @Test def symbolShowModule: Unit = {
    code"""object ${m1}Foo${m2}"""
      .symbol("Foo", (m1 to m2).symInfo("Foo", SymbolKind.Module))
  }

  @Test def symbolShowClassAndCompanion: Unit = {
    code"""object ${m1}Foo${m2}
          |class ${m3}Foo${m4}"""
      .symbol("Foo", (m1 to m2).symInfo("Foo", SymbolKind.Module),
                     (m3 to m4).symInfo("Foo", SymbolKind.Class))
  }

  @Test def multipleProjects0: Unit = {
    val p0 = Project.withSources(
      code"""class ${m1}Foo${m2}"""
    )

    val p1 = Project.dependingOn(p0).withSources(
      code"""class ${m3}Bar${m4} extends Foo"""
    )

    withProjects(p0, p1)
      .symbol("Foo", (m1 to m2).symInfo("Foo", SymbolKind.Class))
  }

  @Test def noLocalSymbols: Unit = {
    code"""object O {
          |   def foo = {
          |     val hello = 0
          |   }
          |}"""
      .symbol("hello")
  }
}
