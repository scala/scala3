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

}
