package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class HoverTest {

  @Test def hoverOnWhiteSpace0: Unit =
    code"$m1 $m2".withSource.hover(m1 to m2, Nil)

  @Test def hoverOnClassShowsDoc: Unit = {
    code"""$m1 /** foo */ ${m2}class Foo $m3 $m4""".withSource
      .hover(m1 to m2, Nil)
      .hover(m2 to m3, List("Foo", "/** foo */"))
      .hover(m3 to m4, Nil)
  }

  @Test def hoverOnClass0: Unit = {
    code"""$m1 ${m2}class Foo $m3 $m4""".withSource
      .hover(m1 to m2, Nil)
      .hover(m2 to m3, "Foo" :: Nil)
      .hover(m3 to m4, Nil)
  }

  @Test def hoverOnClass1: Unit = {
    code"""$m1 ${m2}class Foo { } $m3 $m4""".withSource
      .hover(m1 to m2, Nil)
      .hover(m2 to m3, "Foo" :: Nil)
      .hover(m3 to m4, Nil)
  }

  @Test def hoverOnValDef0: Unit = {
    code"""class Foo {
          |  ${m1}val x = ${m2}8$m3; ${m4}x$m5
          |}""".withSource
      .hover(m1 to m2, "Int" :: Nil)
      .hover(m2 to m3, "Int(8)" :: Nil)
      .hover(m4 to m5, "Int" :: Nil)
  }

  @Test def hoverOnValDef1: Unit = {
    code"""class Foo {
          |  ${m1}final val x = 8$m2; ${m3}x$m4
          |}""".withSource
      .hover(m1 to m2, "Int(8)" :: Nil)
      .hover(m3 to m4, "Int(8)" :: Nil)
  }

  @Test def hoverOnDefDef0: Unit = {
    code"""class Foo {
          |  ${m1}def x = ${m2}8$m3; ${m4}x$m5
          |}""".withSource
      .hover(m1 to m2, "Int" :: Nil)
      .hover(m2 to m3, "Int(8)" :: Nil)
      .hover(m4 to m5, "Int" :: Nil)
  }

  @Test def hoverMissingRef0: Unit = {
    code"""class Foo {
          |  ${m1}x$m2
          |}""".withSource
      .hover(m1 to m2, "<error not found: x>" :: Nil)
  }

  @Test def hoverFun0: Unit = {
    code"""class Foo {
          |  def x: String = $m1"abc"$m2
          |  ${m3}x$m4
          |
          |  def y(): Int = 9
          |  ${m5}y($m6)$m7
          |}
        """.withSource
      .hover(m1 to m2, "String(\"abc\")" :: Nil)
      .hover(m3 to m4, "String" :: Nil)
      .hover(m5 to m6, "(): Int" :: Nil)
      .hover(m6 to m7, "Int" :: Nil)
  }

}
