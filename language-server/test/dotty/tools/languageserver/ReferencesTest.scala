package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class ReferencesTest {

  @Test def valNoReferences0: Unit =
    code"class X { val ${m1}x$m2 = 9 }".withSource.references(m1 to m2, Nil)

  @Test def valReferences0: Unit = {
    code"class X { val ${m1}x$m2 = 9; ${m3}x$m4; ${m5}x$m6 }".withSource
      .references(m1 to m2, List(m3 to m4, m5 to m6))
  }

  @Test def valReferences1: Unit = {
    code"class X { val ${m1}x$m2 = 9; ${m3}x$m4; ${m5}x$m6 }".withSource
      .references(m1 to m2, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
  }

  @Test def classReference0: Unit = {
    code"class ${m1}Foo${m2} { val a = new ${m3}Foo${m4} }".withSource
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def classReference1: Unit = {
    code"class ${m1}Foo${m2}(x: Int) { val a = new ${m3}Foo${m4}(1) }".withSource
      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m3 to m4), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
  }

  @Test def classReferenceCompanion: Unit = {
    code"""class ${m1}Foo${m2}(x: Any)
           object ${m3}Foo${m4} { val bar = new ${m5}Foo${m6}(${m7}Foo${m8}) }""".withSource
      .references(m1 to m2, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m5 to m6), withDecl = false)
      .references(m3 to m4, List(m3 to m4, m7 to m8), withDecl = true)
      .references(m3 to m4, List(m7 to m8), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m5 to m6), withDecl = false)
      .references(m7 to m8, List(m3 to m4, m7 to m8), withDecl = true)
      .references(m7 to m8, List(m7 to m8), withDecl = false)
  }

  @Test def importReference1: Unit = {
    code"""import ${m1}Foo${m2}._
           object ${m3}Foo${m4} { def ${m5}bar${m6}: Int = 0 }
           trait Bar { def buzz = ${m7}bar${m8} }""".withSource

      .references(m1 to m2, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m1 to m2, List(m1 to m2), withDecl = false)
      .references(m3 to m4, List(m1 to m2, m3 to m4), withDecl = true)
      .references(m3 to m4, List(m1 to m2), withDecl = false)
      .references(m5 to m6, List(m5 to m6, m7 to m8), withDecl = true)
      .references(m5 to m6, List(m7 to m8), withDecl = false)
      .references(m7 to m8, List(m5 to m6, m7 to m8), withDecl = true)
      .references(m7 to m8, List(m7 to m8), withDecl = false)
  }

  @Test def importReference2: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4} { object ${m5}Baz${m6} } }
           trait Buzz { import ${m7}Foo${m8}.${m9}Bar${m10}.${m11}Baz${m12} }""".withSource

    .references(m1 to m2, List(m1 to m2, m7 to m8), withDecl = true)
    .references(m1 to m2, List(m7 to m8), withDecl = false)
    .references(m3 to m4, List(m3 to m4, m9 to m10), withDecl = true)
    .references(m3 to m4, List(m9 to m10), withDecl = false)
    .references(m5 to m6, List(m5 to m6, m11 to m12), withDecl = true)
    .references(m5 to m6, List(m11 to m12), withDecl = false)
    .references(m7 to m8, List(m1 to m2, m7 to m8), withDecl = true)
    .references(m7 to m8, List(m7 to m8), withDecl = false)
    .references(m9 to m10, List(m3 to m4, m9 to m10), withDecl = true)
    .references(m9 to m10, List(m9 to m10), withDecl = false)
    .references(m11 to m12, List(m5 to m6, m11 to m12), withDecl = true)
    .references(m11 to m12, List(m11 to m12), withDecl = false)
  }

  @Test def importReference3: Unit = {
    code"""import ${m1}Foo${m2}.${m3}Bar${m4}
           object ${m5}Foo${m6} { object ${m7}Bar${m8} }""".withSource

      .references(m1 to m2, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m1 to m2), withDecl = false)
      .references(m3 to m4, List(m3 to m4, m7 to m8), withDecl = true)
      .references(m3 to m4, List(m3 to m4), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m1 to m2), withDecl = false)
      .references(m7 to m8, List(m3 to m4, m7 to m8), withDecl = true)
      .references(m7 to m8, List(m3 to m4), withDecl = false)
  }

  @Test def importReferenceClassAndCompanion: Unit = {
    code"""object Foo { object ${m1}Bar${m2}; class ${m3}Bar${m4} }
           trait Buzz { import Foo.${m5}Bar${m6} }""".withSource
      .references(m1 to m2, List(m1 to m2, m5 to m6), withDecl = true)
      .references(m1 to m2, List(m5 to m6), withDecl = false)
      .references(m3 to m4, List(m3 to m4, m5 to m6), withDecl = true)
      .references(m3 to m4, List(m5 to m6), withDecl = false)
      .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6), withDecl = true)
      .references(m5 to m6, List(m5 to m6), withDecl = false)
  }

  @Test def importReferenceWithRename: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4} { object ${m5}Baz${m6} } }
           trait Buzz { import ${m7}Foo${m8}.${m9}Bar${m10}.{${m11}Baz${m12} => ${m13}Quux${m14}}""".withSource

    .references(m1 to m2, List(m1 to m2, m7 to m8), withDecl = true)
    .references(m1 to m2, List(m7 to m8), withDecl = false)
    .references(m3 to m4, List(m3 to m4, m9 to m10), withDecl = true)
    .references(m3 to m4, List(m9 to m10), withDecl = false)
    .references(m5 to m6, List(m5 to m6, m11 to m12, m13 to m14), withDecl = true)
    .references(m5 to m6, List(m11 to m12, m13 to m14), withDecl = false)
    .references(m7 to m8, List(m1 to m2, m7 to m8), withDecl = true)
    .references(m7 to m8, List(m7 to m8), withDecl = false)
    .references(m9 to m10, List(m3 to m4, m9 to m10), withDecl = true)
    .references(m9 to m10, List(m9 to m10), withDecl = false)
    .references(m11 to m12, List(m5 to m6, m11 to m12, m13 to m14), withDecl = true)
    .references(m11 to m12, List(m11 to m12, m13 to m14), withDecl = false)
    .references(m13 to m14, List(m5 to m6, m11 to m12, m13 to m14), withDecl = true)
    .references(m13 to m14, List(m11 to m12, m13 to m14), withDecl = false)
  }

  @Test def importReferenceClassAndCompanionWithRename: Unit = {
    code"""object ${m1}Foo${m2} { object ${m3}Bar${m4}; class ${m5}Bar${m6} }
           trait Buzz { import ${m7}Foo${m8}.{${m9}Bar${m10} => ${m11}Baz${m12}} }""".withSource

      .references(m1 to m2, List(m1 to m2, m7 to m8), withDecl = true)
      .references(m1 to m2, List(m7 to m8), withDecl = false)
      .references(m3 to m4, List(m3 to m4, m9 to m10, m11 to m12), withDecl = true)
      .references(m3 to m4, List(m9 to m10, m11 to m12), withDecl = false)
      .references(m5 to m6, List(m5 to m6, m9 to m10, m11 to m12), withDecl = true)
      .references(m5 to m6, List(m9 to m10, m11 to m12), withDecl = false)
      .references(m7 to m8, List(m1 to m2, m7 to m8), withDecl = true)
      .references(m7 to m8, List(m7 to m8), withDecl = false)
      .references(m9 to m10, List(m3 to m4, m5 to m6, m9 to m10, m11 to m12), withDecl = true)
      .references(m9 to m10, List(m9 to m10, m11 to m12), withDecl = false)
      .references(m11 to m12, List(m3 to m4, m5 to m6, m9 to m10, m11 to m12), withDecl = true)
      .references(m11 to m12, List(m9 to m10, m11 to m12), withDecl = false)
  }

  @Test def importReferenceMembers: Unit = {
    code"""object Foo { def ${m1}bar${m2} = 2; type ${m3}bar${m4} = fizz; class fizz }
           trait Quux { import Foo.{${m5}bar${m6} => ${m7}buzz${m8}} }""".withSource

    .references(m1 to m2, List(m1 to m2, m5 to m6, m7 to m8), withDecl = true)
    .references(m1 to m2, List(m5 to m6, m7 to m8), withDecl = false)
    .references(m3 to m4, List(m3 to m4, m5 to m6, m7 to m8), withDecl = true)
    .references(m3 to m4, List(m5 to m6, m7 to m8), withDecl = false)
    .references(m5 to m6, List(m1 to m2, m3 to m4, m5 to m6, m7 to m8), withDecl = true)
    .references(m5 to m6, List(m5 to m6, m7 to m8), withDecl = false)
    .references(m7 to m8, List(m1 to m2, m3 to m4, m5 to m6, m7 to m8), withDecl = true)
    .references(m7 to m8, List(m5 to m6, m7 to m8), withDecl = false)
  }

}
