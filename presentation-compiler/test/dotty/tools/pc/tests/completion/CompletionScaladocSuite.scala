package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class CompletionScaladocSuite extends BaseCompletionSuite:

  @Test def `methoddef-label` =
    check(
      """
        |object A {
        |  /**@@
        |  def test(x: Int, y: Int): Int = ???
        |}""".stripMargin,
      """|/** */Scaladoc Comment
         |""".stripMargin
    )

  @Test def `classdef-label` =
    check(
      """
        |object A {
        |  /**@@
        |  case class(x: Int) {}
        |}""".stripMargin,
      """|/** */Scaladoc Comment
         |""".stripMargin
    )

  // see: https://github.com/scalameta/metals/issues/1941
  @Test def `no-associated-def-label` =
    check(
      """
        |object A {
        |  /**@@
        |}""".stripMargin,
      """|/** */Scaladoc Comment
         |""".stripMargin
    )

  @Test def `methoddef` =
    checkEdit(
      """|object A {
         |  /**@@
         |  def test1(param1: Int, param2: Int): Int = ???
         |  def test2(param1: Int, param2: Int, param3: Int): Int = ???
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    *
         |    * @param param1
         |    * @param param2
         |    * @return
         |    */
         |  def test1(param1: Int, param2: Int): Int = ???
         |  def test2(param1: Int, param2: Int, param3: Int): Int = ???
         |}
         |""".stripMargin
    )

  @Test def `classdef` =
    checkEdit(
      """|object A {
         |  /**@@
         |  class Test1(param1: Int, param2: Int) {}
         |  class Test2(param1: Int, param2: Int, param3: Int) {}
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    *
         |    * @param param1
         |    * @param param2
         |    */
         |  class Test1(param1: Int, param2: Int) {}
         |  class Test2(param1: Int, param2: Int, param3: Int) {}
         |}
         |""".stripMargin
    )

  @Test def `valdef` =
    checkEdit(
      """|object A {
         |  /**@@
         |  val x = 1
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    */
         |  val x = 1
         |}
         |""".stripMargin
    )

  @Test def `objectdef` =
    checkEdit(
      """|/**@@
         |object A {
         |  // do not calculate scaladoc based on the method
         |  def test(x: Int): Int = ???
         |}
         |""".stripMargin,
      """|/**
         |  * $0
         |  */
         |object A {
         |  // do not calculate scaladoc based on the method
         |  def test(x: Int): Int = ???
         |}
         |""".stripMargin
    )

  @Test def `defdef-nested` =
    checkEdit(
      """|object A {
         |  def test(x: Int): Int = {
         |    /**@@
         |    def nest(y: Int) = ???
         |  }
         |}
         |""".stripMargin,
      """|object A {
         |  def test(x: Int): Int = {
         |    /**
         |      * $0
         |      *
         |      * @param y
         |      * @return
         |      */
         |    def nest(y: Int) = ???
         |  }
         |}
         |""".stripMargin
    )

  @Test def `classdef-nested` =
    checkEdit(
      """|object A {
         |  /**@@
         |  case class B(x: Int) {
         |    case class C(y: Int) {}
         |  }
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    *
         |    * @param x
         |    */
         |  case class B(x: Int) {
         |    case class C(y: Int) {}
         |  }
         |}
         |""".stripMargin
    )

  @Test def `trait-classdef-nested` =
    checkEdit(
      """|object A {
         |  /**@@
         |  trait B {
         |    // do not complete scaladef for class C
         |    case class C(y: Int) {}
         |  }
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    */
         |  trait B {
         |    // do not complete scaladef for class C
         |    case class C(y: Int) {}
         |  }
         |}
         |""".stripMargin
    )

  @Test def `defdef-no-param-cursor` =
    checkEdit(
      """|object A {
         |  /**@@
         |  def test1: Int = ???
         |  def test2(param1: Int, param2: Int, param3: Int): Int = ???
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    *
         |    * @return
         |    */
         |  def test1: Int = ???
         |  def test2(param1: Int, param2: Int, param3: Int): Int = ???
         |}
         |""".stripMargin
    )

  @Test def `defdef-returns-unit` =
    checkEdit(
      """|// Don't add @return line for a method whose return type is Unit.
         |object A {
         |  /**@@
         |  def test(param1: Int, param2: Int): Unit = ???
         |}
         |""".stripMargin,
      """|// Don't add @return line for a method whose return type is Unit.
         |object A {
         |  /**
         |    * $0
         |    *
         |    * @param param1
         |    * @param param2
         |    */
         |  def test(param1: Int, param2: Int): Unit = ???
         |}
         |""".stripMargin
    )

  @Test def `defdef-returns-inferred-unit` =
    checkEdit(
      """|// Don't add @return line for a method whose return type is Unit.
         |object A {
         |  /**@@
         |  def test(param1: Int, param2: Int) = {}
         |}
         |""".stripMargin,
      """|// Don't add @return line for a method whose return type is Unit.
         |object A {
         |  /**
         |    * $0
         |    *
         |    * @param param1
         |    * @param param2
         |    */
         |  def test(param1: Int, param2: Int) = {}
         |}
         |""".stripMargin
    )

  @Test def `defdef-evidence` =
    checkEdit(
      """|// do not add compiler generated param like `evidence$1`
         |object A {
         |  /**@@
         |  def test[T: Ordering](x: T, y: T): T = if(x < y) x else y
         |}
         |""".stripMargin,
      """|// do not add compiler generated param like `evidence$1`
         |object A {
         |  /**
         |    * $0
         |    *
         |    * @param x
         |    * @param y
         |    * @return
         |    */
         |  def test[T: Ordering](x: T, y: T): T = if(x < y) x else y
         |}
         |""".stripMargin
    )

  @Test def `classdef-evidence` =
    checkEdit(
      """|object A {
         |  /**@@
         |  case class Test[T: Ordering](x: T, y: T) {}
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    *
         |    * @param x
         |    * @param y
         |    */
         |  case class Test[T: Ordering](x: T, y: T) {}
         |}
         |""".stripMargin
    )

  @Test def `classdef-curried` =
    checkEdit(
      """|object A {
         |  /**@@
         |  case class Test(a: Int, b: String)(c: Long)
         |}
         |""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    *
         |    * @param a
         |    * @param b
         |    * @param c
         |    */
         |  case class Test(a: Int, b: String)(c: Long)
         |}
         |""".stripMargin
    )

  @Test def `no-associated-def` =
    checkEdit(
      """|object A {
         |  /**@@
         |}""".stripMargin,
      """|object A {
         |  /**
         |    * $0
         |    */
         |}
         |""".stripMargin
    )

  @Test def `extension` =
    checkEdit(
      """|extension (str: String)
         |  /**@@
         |  def foo(param1: Int): Int = ???
         |""".stripMargin,
      """|extension (str: String)
         |  /**
         |    * $0
         |    *
         |    * @param param1
         |    * @return
         |    */
         |  def foo(param1: Int): Int = ???
         |""".stripMargin
    )

  @Test def `anonymous-given` =
    checkEdit(
      """|/**@@
         |def foo(param1: Int)(using String): Int = ???
         |""".stripMargin,
      """|/**
         |  * $0
         |  *
         |  * @param param1
         |  * @return
         |  */
         |def foo(param1: Int)(using String): Int = ???
         |""".stripMargin
    )

  @Test def `named-given` =
    checkEdit(
      """|/**@@
         |def foo(param1: Int)(using s: String): Int = ???
         |""".stripMargin,
      """|/**
         |  * $0
         |  *
         |  * @param param1
         |  * @param s
         |  * @return
         |  */
         |def foo(param1: Int)(using s: String): Int = ???
         |""".stripMargin
    )
