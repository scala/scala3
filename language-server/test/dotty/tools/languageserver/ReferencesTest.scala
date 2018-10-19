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

}
