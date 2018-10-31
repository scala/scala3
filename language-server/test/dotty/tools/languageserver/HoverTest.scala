package dotty.tools.languageserver

import org.junit.Test

import dotty.tools.languageserver.util.Code._

class HoverTest {
  def hoverContent(typeInfo: String, comment: String = ""): Option[String] =
    Some((
      if (comment == "")
        s"""```scala
           |$typeInfo
           |```"""
      else
        s"""```scala
           |$typeInfo
           |$comment
           |```""").stripMargin)

  @Test def hoverOnWhiteSpace0: Unit =
    code"$m1 $m2".withSource.hover(m1 to m2, None)

  @Test def hoverOnClassShowsDoc: Unit = {
    code"""$m1 /** foo */ ${m2}class Foo $m3 $m4""".withSource
      .hover(m1 to m2, None)
      .hover(m2 to m3, hoverContent("Foo", "/** foo */"))
      .hover(m3 to m4, None)
  }

  @Test def hoverOnClass0: Unit = {
    code"""$m1 ${m2}class Foo $m3 $m4""".withSource
      .hover(m1 to m2, None)
      .hover(m2 to m3, hoverContent("Foo"))
      .hover(m3 to m4, None)
  }

  @Test def hoverOnClass1: Unit = {
    code"""$m1 ${m2}class Foo { } $m3 $m4""".withSource
      .hover(m1 to m2, None)
      .hover(m2 to m3, hoverContent("Foo"))
      .hover(m3 to m4, None)
  }

  @Test def hoverOnValDef0: Unit = {
    code"""class Foo {
          |  ${m1}val x = ${m2}8$m3; ${m4}x$m5
          |}""".withSource
      .hover(m1 to m2, hoverContent("Int"))
      .hover(m2 to m3, hoverContent("Int(8)"))
      .hover(m4 to m5, hoverContent("Int"))
  }

  @Test def hoverOnValDef1: Unit = {
    code"""class Foo {
          |  ${m1}final val x = 8$m2; ${m3}x$m4
          |}""".withSource
      .hover(m1 to m2, hoverContent("Int(8)"))
      .hover(m3 to m4, hoverContent("Int(8)"))
  }

  @Test def hoverOnDefDef0: Unit = {
    code"""class Foo {
          |  ${m1}def x = ${m2}8$m3; ${m4}x$m5
          |}""".withSource
      .hover(m1 to m2, hoverContent("Int"))
      .hover(m2 to m3, hoverContent("Int(8)"))
      .hover(m4 to m5, hoverContent("Int"))
  }

  @Test def hoverMissingRef0: Unit = {
    code"""class Foo {
          |  ${m1}x$m2
          |}""".withSource
      .hover(m1 to m2, None)
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
      .hover(m1 to m2, hoverContent("String(\"abc\")" ))
      .hover(m3 to m4, hoverContent("String"))
      .hover(m5 to m6, hoverContent("(): Int"))
      .hover(m6 to m7, hoverContent("Int"))
  }

  @Test def documentationIsCooked: Unit = {
    code"""/** A class: $$Variable
          | *  @define Variable Test
          | */
          |class ${m1}Foo${m2}
          |/** $$Variable */
          |class ${m3}Bar${m4} extends Foo
        """.withSource
      .hover(m1 to m2, hoverContent("Foo", "/** A class: Test\n *  */"))
      .hover(m3 to m4, hoverContent("Bar", "/** Test */"))
  }

}
