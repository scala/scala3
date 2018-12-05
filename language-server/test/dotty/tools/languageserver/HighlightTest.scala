package dotty.tools.languageserver

import org.junit.Test
import dotty.tools.languageserver.util.Code._
import org.eclipse.lsp4j.DocumentHighlightKind.Read

class HighlightTest {

  @Test def valHighlight0: Unit = {
    val xDef = (m1 to m2).withCode("x")
    code"class X { val $xDef = 9 }".withSource
      .highlight(xDef.range, (xDef.range, Read))
  }

  @Test def valHighlight1: Unit = {
    val xDef = (m1 to m2).withCode("x")
    val xRef = (m3 to m4).withCode("x")
    code"class X { val $xDef = 9; $xRef}".withSource
      .highlight(xRef.range, (xDef.range, Read), (xRef.range, Read))
  }

  @Test def highlightClass(): Unit = {
    code"""class ${m1}Foo${m2} { new ${m3}Foo${m4} }""".withSource
      .highlight(m1 to m2, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m3 to m4, (m1 to m2, Read), (m3 to m4, Read))
  }

  @Test def importHighlight0: Unit = {
    code"""object ${m1}Foo${m2} { def ${m5}bar${m6}: Int = 0 }
           trait Bar { import ${m3}Foo${m4}._; def buzz = ${m7}bar${m8} }
           trait Baz { def ${m9}bar${m10}: Int = 1 }""".withSource

      .highlight(m1 to m2, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m3 to m4, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m5 to m6, (m5 to m6, Read), (m7 to m8, Read))
      .highlight(m7 to m8, (m5 to m6, Read), (m7 to m8, Read))
      .highlight(m9 to m10, (m9 to m10, Read))
  }

  @Test def importHighlight1: Unit = {
    code"""import ${m1}Foo${m2}._
           object ${m3}Foo${m4} { def ${m5}bar${m6}: Int = 0 }
           trait Bar { def buzz = ${m7}bar${m8} }""".withSource

      .highlight(m1 to m2, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m3 to m4, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m5 to m6, (m5 to m6, Read), (m7 to m8, Read))
      .highlight(m7 to m8, (m5 to m6, Read), (m7 to m8, Read))
  }

  @Test def importHighlight2: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4} { object ${m5}Baz${m6} } }
           trait Buzz { import ${m7}Foo${m8}.${m9}Bar${m10}.${m11}Baz${m12} }""".withSource

    .highlight(m1 to m2, (m1 to m2, Read), (m7 to m8, Read))
    .highlight(m3 to m4, (m3 to m4, Read), (m9 to m10, Read))
    .highlight(m5 to m6, (m5 to m6, Read), (m11 to m12, Read))
    .highlight(m7 to m8, (m1 to m2, Read), (m7 to m8, Read))
    .highlight(m9 to m10, (m3 to m4, Read), (m9 to m10, Read))
    .highlight(m11 to m12, (m5 to m6, Read), (m11 to m12, Read))
  }

  @Test def importHighlight3: Unit = {
    code"""import ${m1}Foo${m2}.${m3}Bar${m4}
           object ${m5}Foo${m6} { object ${m7}Bar${m8} }""".withSource

      .highlight(m1 to m2, (m1 to m2, Read), (m5 to m6, Read))
      .highlight(m3 to m4, (m3 to m4, Read), (m7 to m8, Read))
      .highlight(m5 to m6, (m1 to m2, Read), (m5 to m6, Read))
      .highlight(m7 to m8, (m3 to m4, Read), (m7 to m8, Read))
  }

  @Test def importHighlightClassAndCompanion: Unit = {
    code"""object Foo { object ${m1}Bar${m2}; class ${m3}Bar${m4} }
           trait Buzz { import Foo.${m5}Bar${m6} }""".withSource
      .highlight(m1 to m2, (m1 to m2, Read), (m5 to m6, Read))
      .highlight(m3 to m4, (m3 to m4, Read), (m5 to m6, Read))
      .highlight(m5 to m6, (m3 to m4, Read), (m5 to m6, Read), (m1 to m2, Read))
  }

  @Test def importHighlightWithRename: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4} { object ${m5}Baz${m6} } }
           trait Buzz { import ${m7}Foo${m8}.${m9}Bar${m10}.{${m11}Baz${m12} => ${m13}Quux${m14}}""".withSource

    .highlight(m1 to m2, (m1 to m2, Read), (m7 to m8, Read))
    .highlight(m3 to m4, (m3 to m4, Read), (m9 to m10, Read))
    .highlight(m5 to m6, (m5 to m6, Read), (m11 to m12, Read), (m13 to m14, Read))
    .highlight(m7 to m8, (m1 to m2, Read), (m7 to m8, Read))
    .highlight(m9 to m10, (m3 to m4, Read), (m9 to m10, Read))
    .highlight(m11 to m12, (m5 to m6, Read), (m11 to m12, Read), (m13 to m14, Read))
    .highlight(m13 to m14, (m5 to m6, Read), (m11 to m12, Read), (m13 to m14, Read))
  }

  @Test def importHighlightClassAndCompanionWithRename: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4}; class ${m5}Bar${m6} }
           trait Buzz { import ${m7}Foo${m8}.{${m9}Bar${m10} => ${m11}Baz${m12}} }""".withSource

      .highlight(m1 to m2, (m1 to m2, Read), (m7 to m8, Read))
      .highlight(m3 to m4, (m3 to m4, Read), (m9 to m10, Read), (m11 to m12, Read))
      .highlight(m5 to m6, (m5 to m6, Read), (m9 to m10, Read), (m11 to m12, Read))
      .highlight(m7 to m8, (m1 to m2, Read), (m7 to m8, Read))
      .highlight(m9 to m10, (m3 to m4, Read), (m5 to m6, Read), (m9 to m10, Read), (m11 to m12, Read))
      .highlight(m11 to m12, (m3 to m4, Read), (m5 to m6, Read), (m9 to m10, Read), (m11 to m12, Read))
  }

  @Test def importHighlightMembers: Unit = {
    code"""object Foo { def ${m1}bar${m2} = 2; type ${m3}bar${m4} = fizz; class fizz }
           trait Quux { import Foo.{${m5}bar${m6} => ${m7}buzz${m8}} }""".withSource

    .highlight(m1 to m2, (m1 to m2, Read), (m5 to m6, Read), (m7 to m8, Read))
    .highlight(m3 to m4, (m3 to m4, Read), (m5 to m6, Read), (m7 to m8, Read))
    .highlight(m5 to m6, (m1 to m2, Read), (m3 to m4, Read), (m5 to m6, Read), (m7 to m8, Read))
    .highlight(m7 to m8, (m1 to m2, Read), (m3 to m4, Read), (m5 to m6, Read), (m7 to m8, Read))
  }

  @Test def multipleImportsPerLineWithRename: Unit = {
    withSources(
      code"""object A { class ${m1}B${m2}; class ${m3}C${m4} }
             import A.{${m5}B${m6} => ${m7}B2${m8}, ${m9}C${m10} => ${m11}C2${m12}}
             class E"""
    ).highlight(m1 to m2, (m1 to m2, Read), (m5 to m6, Read), (m7 to m8, Read))
     .highlight(m3 to m4, (m3 to m4, Read), (m9 to m10, Read), (m11 to m12, Read))
     .highlight(m5 to m6, (m1 to m2, Read), (m5 to m6, Read), (m7 to m8, Read))
     .highlight(m7 to m8, (m1 to m2, Read), (m5 to m6, Read), (m7 to m8, Read))
     .highlight(m9 to m10, (m3 to m4, Read), (m9 to m10, Read), (m11 to m12, Read))
     .highlight(m11 to m12, (m3 to m4, Read), (m9 to m10, Read), (m11 to m12, Read))
  }

  @Test def thisAndSelf: Unit = {
    code"""class ${m1}A${m2} { ${m3}self${m4} =>
          |  def foo = ${m5}this${m6}
          |  def bar = ${m7}self${m8}
          |}""".withSource
      .highlight(m1 to m2, (m1 to m2, Read))
      .highlight(m3 to m4, (m3 to m4, Read), (m5 to m6, Read), (m7 to m8, Read))
      .highlight(m5 to m6, (m3 to m4, Read), (m5 to m6, Read), (m7 to m8, Read))
      .highlight(m7 to m8, (m3 to m4, Read), (m5 to m6, Read), (m7 to m8, Read))
  }

  @Test def nestedSelf: Unit = {
    code"""class A { ${m1}self${m2} =>
             val bar = ${m3}self${m4}
             class B { ${m5}self${m6} =>
               val foo = ${m7}self${m8}
             }
           }""".withSource
      .highlight(m1 to m2, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m3 to m4, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m5 to m6, (m5 to m6, Read), (m7 to m8, Read))
      .highlight(m7 to m8, (m5 to m6, Read), (m7 to m8, Read))
  }

  @Test def highlightThis: Unit = {
    code"""class A {
          |  val foo = ${m1}this${m2}
          |  val bar = ${m3}this${m4}
          |}""".withSource
      .highlight(m1 to m2, (m1 to m2, Read), (m3 to m4, Read))
      .highlight(m3 to m4, (m1 to m2, Read), (m3 to m4, Read))
  }

}
