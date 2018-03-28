package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class DefinitionTest {

  @Test def classDefinitionNotFound0: Unit =
    code"class Foo { new ${m1}Bar$m2 }".withSource.definition(m1 to m2, Nil)

  @Test def classDefinition0: Unit = {
    withSources(
      code"class ${m1}Foo$m2 { new Foo }",
      code"class Bar { val foo: ${m3}Foo$m4 = new ${m5}Foo$m6 }"
    ) .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m1 to m2))
      .definition(m5 to m6, List(m1 to m2))
  }

  @Test def valDefinition0: Unit = {
    withSources(
      code"class Foo { val ${m1}x$m2 = 0; ${m3}x$m4 }",
      code"class Bar { val foo = new Foo; foo.${m5}x$m6 }"
    ) .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m1 to m2))
      .definition(m5 to m6, List(m1 to m2))
  }

  @Test def defDefinition0: Unit = {
    withSources(
      code"class Foo { def ${m1}x$m2 = 0; ${m3}x$m4 }",
      code"class Bar { val foo = new Foo; foo.${m5}x$m6 }"
    ) .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m1 to m2))
      .definition(m5 to m6, List(m1 to m2))
  }

}
