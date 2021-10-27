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
           |```
           |$comment""").stripMargin)

  @Test def hoverOnWhiteSpace0: Unit =
    code"$m1 $m2".withSource.hover(m1 to m2, None)

  @Test def hoverOnClassShowsDoc: Unit = {
    code"""$m1 /** foo */ ${m2}class Foo $m3 $m4""".withSource
      .hover(m1 to m2, None)
      .hover(m2 to m3, hoverContent("Foo", "foo"))
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
      .hover(m2 to m3, hoverContent("(8 : Int)"))
      .hover(m4 to m5, hoverContent("Int"))
  }

  @Test def hoverOnValDef1: Unit = {
    code"""class Foo {
          |  ${m1}final val x = 8$m2; ${m3}x$m4
          |}""".withSource
      .hover(m1 to m2, hoverContent("(8 : Int)"))
      .hover(m3 to m4, hoverContent("(8 : Int)"))
  }

  @Test def hoverOnDefDef0: Unit = {
    code"""class Foo {
          |  ${m1}def x = ${m2}8$m3; ${m4}x$m5
          |}""".withSource
      .hover(m1 to m2, hoverContent("Int"))
      .hover(m2 to m3, hoverContent("(8 : Int)"))
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
      .hover(m1 to m2, hoverContent("(\"abc\" : String)"))
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
      .hover(m1 to m2, hoverContent("Foo", "A class: Test"))
      .hover(m3 to m4, hoverContent("Bar", "Test"))
  }

  @Test def documentationIsFormatted: Unit = if (!scala.util.Properties.isWin) {
    code"""class Foo(val x: Int, val y: Int) {
          |  /**
          |   * Does something
          |   *
          |   * @tparam T A first type param
          |   * @tparam U Another type param
          |   * @param fizz Again another number
          |   * @param buzz A String
          |   * @param ev   An implicit boolean
          |   * @return Something
          |   * @throws java.lang.NullPointerException if you're unlucky
          |   * @throws java.lang.InvalidArgumentException if the argument is invalid
          |   * @see java.nio.file.Paths#get()
          |   * @note A note
          |   * @example myFoo.bar[Int, String](0, "hello, world")
          |   * @author John Doe
          |   * @version 1.0
          |   * @since 0.1
          |   * @usecase def bar(fizz: Int, buzz: String): Any
          |   */
          |  def ${m1}bar${m2}[T, U](fizz: Int, buzz: String)(implicit ev: Boolean): Any = ???
          |}""".withSource
      .hover(
        m1 to m2,
        hoverContent("[T, U](fizz: Int, buzz: String)(implicit ev: Boolean): Any",
                     """Does something
                       |
                       |**Type Parameters**
                       | - **T** A first type param
                       | - **U** Another type param
                       |
                       |**Parameters**
                       | - **fizz** Again another number
                       | - **buzz** A String
                       | - **ev** An implicit boolean
                       |
                       |**Returns**
                       | - Something
                       |
                       |**Throws**
                       | - **java.lang.NullPointerException** if you're unlucky
                       | - **java.lang.InvalidArgumentException** if the argument is invalid
                       |
                       |**See Also**
                       | - java.nio.file.Paths#get()
                       |
                       |**Examples**
                       | - ```scala
                       |   myFoo.bar[Int, String](0, "hello, world")
                       |   ```
                       |
                       |**Note**
                       | - A note
                       |
                       |**Authors**
                       | - John Doe
                       |
                       |**Since**
                       | - 0.1
                       |
                       |**Version**
                       | - 1.0""".stripMargin))
  }

  @Test def i5482: Unit = {
    code"""object Test {
          |  def bar: Int = 2 / 1
          |  /** hello */
          |  def ${m1}baz${m2}: Int = ???
          |}""".withSource
      .hover(m1 to m2, hoverContent("Int", "hello"))
  }

  @Test def i4678: Unit = {
    code"""class Foo {
          |  val x: Int = (${m1}1:${m2} ${m3}@annot1${m4} ${m5}@annot2${m6} ${m7}@annot3${m8} ${m9}@annot4${m10} ${m11}@annot5${m12})
          |}
          |class annot1 extends scala.annotation.Annotation
          |class annot2 extends scala.annotation.Annotation
          |class annot3 extends scala.annotation.Annotation
          |class annot4 extends scala.annotation.Annotation
          |class annot5 extends scala.annotation.Annotation
          |""".withSource
      .hover(m1 to m2, hoverContent("(1 : Int)"))
      .hover(m3 to m4, hoverContent("annot1"))
      .hover(m5 to m6, hoverContent("annot2"))
      .hover(m7 to m8, hoverContent("annot3"))
      .hover(m9 to m10, hoverContent("annot4"))
      .hover(m11 to m12, hoverContent("annot5"))
  }

  @Test def unicodeChar: Unit = {
    code"""object Test {
          |  type →
          |  type `🤪`
          |  def ${m1}bar${m2}: → = ???
          |  def ${m3}baz${m4}: `🤪` = ???
          |}""".withSource
      .hover(m1 to m2, hoverContent("Test.→"))
      .hover(m3 to m4, hoverContent("Test.🤪"))

  }

  @Test def topLevel: Unit = {
    code"""package hello
          |val x: Int = 1
          |val y = ${m1}this${m2}.x""".withSource
      // The test framework will place the code above in a virtual file called Source0.scala,
      // sp the top-level definitions should be enclosed in an object called `Source0$package`.
      .hover(m1 to m2, hoverContent("(hello.Source0$package : hello.Source0$package.type)"))
  }
}
