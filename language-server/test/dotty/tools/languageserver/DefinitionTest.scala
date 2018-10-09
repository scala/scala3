package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.embedded.CodeMarker

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

  @Test def liftedExpression: Unit = {
    withSources(
      code"class ${m1}A${m2}",
      code"object B { val lst = new ${m3}A${m4} :: Nil }"
    ).definition(m3 to m4, List(m1 to m2))
  }

  @Test def goToDefNamedArg: Unit = {
    code"""object Foo {
             def foo(${m1}x${m2}: Int) = ${m3}x${m4}
             foo(${m5}x${m6} = 2)
           }""".withSource
      .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m1 to m2))
      .definition(m5 to m6, List(m1 to m2))
  }

  @Test def goToDefNamedArgOverload: Unit = {

    code"""object Foo {
             def foo(${m1}x${m2}: String): String = ${m3}x${m4}
             def foo(${m5}x${m6}: Int): String = foo(${m7}x${m8} = ${m9}x${m10}.toString)
             foo(${m11}x${m12} = "a")
             foo(${m13}x${m14} = 2)
           }""".withSource
      .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m1 to m2))
      .definition(m5 to m6, List(m5 to m6))
      .definition(m7 to m8, List(m1 to m2))
      .definition(m9 to m10, List(m5 to m6))
      .definition(m11 to m12, List(m1 to m2))
      .definition(m13 to m14, List(m5 to m6))
  }

  @Test def goToConstructorNamedArg: Unit = {
    withSources(
      code"""class Foo(${m1}x${m2}: Int)""",
      code"""class Bar extends Foo(${m3}x${m4} = 5)""",
      code"""object Buzz { new Foo(${m5}x${m6} = 2) }"""
    ) .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m1 to m2))
      .definition(m5 to m6, List(m1 to m2))
  }

  @Test def goToConstructorNamedArgOverload: Unit = {

    withSources(
      code"""class Foo(${m1}x${m2}: String) {
               def this(${m3}x${m4}: Int) = this(${m5}x${m6} = ${m7}x${m8}.toString)
             }""",
      code"""object Bar {
               new Foo(${m9}x${m10} = 1)
               new Foo(${m11}x${m12} = "a")
             }"""
    ) .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m3 to m4))
      .definition(m5 to m6, List(m1 to m2))
      .definition(m7 to m8, List(m3 to m4))
      .definition(m9 to m10, List(m3 to m4))
      .definition(m11 to m12, List(m1 to m2))
  }

  @Test def goToParamCopyMethod: Unit = {

    withSources(
      code"""case class Foo(${m1}x${m2}: Int, ${m3}y${m4}: String)""",
      code"""object Bar {
               Foo(0, "a").copy(${m5}x${m6} = 1, ${m7}y${m8} = "b")
               Foo(2, "c").copy(${m9}y${m10} = "d")"""
    ) .definition(m1 to m2, List(m1 to m2))
      .definition(m3 to m4, List(m3 to m4))
      .definition(m5 to m6, List(m1 to m2))
      .definition(m7 to m8, List(m3 to m4))
      .definition(m9 to m10, List(m3 to m4))
  }

}
