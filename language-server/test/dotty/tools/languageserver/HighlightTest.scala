package dotty.tools.languageserver

import org.junit.Test
import dotty.tools.languageserver.util.Code._
import org.eclipse.lsp4j.DocumentHighlightKind

class HighlightTest {

  @Test def valHighlight0: Unit = {
    val xDef = (m1 to m2).withCode("x")
    code"class X { val $xDef = 9 }"
      .highlight(xDef.range, (xDef.range, DocumentHighlightKind.Read))
  }

  @Test def valHighlight1: Unit = {
    val xDef = (m1 to m2).withCode("x")
    val xRef = (m3 to m4).withCode("x")
    code"class X { val $xDef = 9; $xRef}"
      .highlight(xRef.range, (xDef.range, DocumentHighlightKind.Read), (xRef.range, DocumentHighlightKind.Read))
  }

  @Test def highlightClass(): Unit = {
    code"""class ${m1}Foo${m2} { new ${m3}Foo${m4} }"""
      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
  }

  @Test def importHighlight0: Unit = {
    code"""object ${m1}Foo${m2} { def ${m5}bar${m6}: Int = 0 }
           trait Bar { import ${m3}Foo${m4}._; def buzz = ${m7}bar${m8} }
           trait Baz { def ${m9}bar${m10}: Int = 1 }"""

      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
      .highlight(m5 to m6, (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m7 to m8, (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m9 to m10, (m9 to m10, DocumentHighlightKind.Read))
  }

  @Test def importHighlight1: Unit = {
    code"""import ${m1}Foo${m2}._
           object ${m3}Foo${m4} { def ${m5}bar${m6}: Int = 0 }
           trait Bar { def buzz = ${m7}bar${m8} }"""

      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read))
      .highlight(m5 to m6, (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m7 to m8, (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
  }

  @Test def importHighlight2: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4} { object ${m5}Baz${m6} } }
           trait Buzz { import ${m7}Foo${m8}.${m9}Bar${m10}.${m11}Baz${m12} }"""

    .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read))
    .highlight(m5 to m6, (m5 to m6, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
    .highlight(m7 to m8, (m1 to m2, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m9 to m10, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read))
    .highlight(m11 to m12, (m5 to m6, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
  }

  @Test def importHighlight3: Unit = {
    code"""import ${m1}Foo${m2}.${m3}Bar${m4}
           object ${m5}Foo${m6} { object ${m7}Bar${m8} }"""

      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m5 to m6, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read))
      .highlight(m7 to m8, (m3 to m4, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
  }

  @Test def importHighlightClassAndCompanion: Unit = {
    code"""object Foo { object ${m1}Bar${m2}; class ${m3}Bar${m4} }
           trait Buzz { import Foo.${m5}Bar${m6} }"""
      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read))
      .highlight(m5 to m6, (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m1 to m2, DocumentHighlightKind.Read))
  }

  @Test def importHighlightWithRename: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4} { object ${m5}Baz${m6} } }
           trait Buzz { import ${m7}Foo${m8}.${m9}Bar${m10}.{${m11}Baz${m12} => ${m13}Quux${m14}}"""

    .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read))
    .highlight(m5 to m6, (m5 to m6, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read), (m13 to m14, DocumentHighlightKind.Read))
    .highlight(m7 to m8, (m1 to m2, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m9 to m10, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read))
    .highlight(m11 to m12, (m5 to m6, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read), (m13 to m14, DocumentHighlightKind.Read))
    .highlight(m13 to m14, (m5 to m6, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read), (m13 to m14, DocumentHighlightKind.Read))
  }

  @Test def importHighlightClassAndCompanionWithRename: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4}; class ${m5}Bar${m6} }
           trait Buzz { import ${m7}Foo${m8}.{${m9}Bar${m10} => ${m11}Baz${m12}} }"""

      .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
      .highlight(m5 to m6, (m5 to m6, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
      .highlight(m7 to m8, (m1 to m2, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
      .highlight(m9 to m10, (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
      .highlight(m11 to m12, (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
  }

  @Test def importHighlightMembers: Unit = {
    code"""object Foo { def ${m1}bar${m2} = 2; type ${m3}bar${m4} = fizz; class fizz }
           trait Quux { import Foo.{${m5}bar${m6} => ${m7}buzz${m8}} }"""

    .highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m5 to m6, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
    .highlight(m7 to m8, (m1 to m2, DocumentHighlightKind.Read), (m3 to m4, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
  }

  @Test def multipleImportsPerLineWithRename: Unit = {
    withSources(
      code"""object A { class ${m1}B${m2}; class ${m3}C${m4} }
             import A.{${m5}B${m6} => ${m7}B2${m8}, ${m9}C${m10} => ${m11}C2${m12}}
             class E"""
    ).highlight(m1 to m2, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
     .highlight(m3 to m4, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
     .highlight(m5 to m6, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
     .highlight(m7 to m8, (m1 to m2, DocumentHighlightKind.Read), (m5 to m6, DocumentHighlightKind.Read), (m7 to m8, DocumentHighlightKind.Read))
     .highlight(m9 to m10, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
     .highlight(m11 to m12, (m3 to m4, DocumentHighlightKind.Read), (m9 to m10, DocumentHighlightKind.Read), (m11 to m12, DocumentHighlightKind.Read))
  }

}
