package dotty.tools.pc.tests.hover

import dotty.tools.pc.base.BaseHoverSuite

import org.junit.Test

class HoverDefnSuite extends BaseHoverSuite:
  @Test def `val` =
    check(
      """object a {
        |  <<val @@x = List(1)>>
        |}
        |""".stripMargin,
      """|val x: List[Int]
         |""".stripMargin.hover
    )

  @Test def `var` =
    check(
      """object a {
        |  <<var @@x = List(1)>>
        |}
        |""".stripMargin,
      """|var x: List[Int]
         |""".stripMargin.hover
    )

  @Test def `def-nullary` =
    check(
      """object a {
        |  <<def @@x = List(1)>>
        |}
        |""".stripMargin,
      """|def x: List[Int]
         |""".stripMargin.hover
    )

  @Test def `def-params` =
    check(
      """object a {
        |  <<def @@method(x: Int) = List(x)>>
        |}
        |""".stripMargin,
      """|def method(x: Int): List[Int]
         |""".stripMargin.hover
    )

  @Test def `def-tparams` =
    check(
      """object a {
        |  <<def @@empty[T] = Option.empty[T]>>
        |}
        |""".stripMargin,
      """|def empty[T]: Option[T]
         |""".stripMargin.hover
    )

  @Test def `context-bound` =
    check(
      """object a {
        |  <<def @@empty[T:Ordering] = Option.empty[T]>>
        |}
        |""".stripMargin,
      "def empty[T: Ordering]: Option[T]".hover
    )

  @Test def `lambda-param` =
    check(
      """object a {
        |  List(1).map(<<@@x>> => )
        |}
        |""".stripMargin,
      """|```scala
         |x: Int
         |```
         |""".stripMargin
    )

  @Test def `param` =
    check(
      """object a {
        |  def method(<<@@x: Int>>): Int = x
        |}
        |""".stripMargin,
      """|```scala
         |x: Int
         |```
         |""".stripMargin
    )

  @Test def `ctor` =
    check(
      """class a {
        |  <<def t@@his(x: Int) = this()>>
        |}
        |""".stripMargin,
      """|```scala
         |def this(x: Int): a
         |```
         |""".stripMargin
    )

  @Test def `ctor-param` =
    check(
      """class a {
        |  def this(<<@@x: Int>>) = this()
        |}
        |""".stripMargin,
      """|```scala
         |x: Int
         |```
         |""".stripMargin
    )

  @Test def `implicit-param` =
    check(
      """class a {
        |  def method(implicit <<@@x: Int>>) = this()
        |}
        |""".stripMargin,
      """|```scala
         |implicit x: Int
         |```
         |""".stripMargin
    )

  @Test def `implicit-param2` =
    check(
      """class a {
        |  def method(implicit y: Int, <<@@x: Int>>) = this()
        |}
        |""".stripMargin,
      """|```scala
         |implicit x: Int
         |```
         |""".stripMargin
    )

  @Test def `object` =
    check(
      """package `object`
        |object M@@yObject
        |""".stripMargin,
      "object MyObject: `object`".hover
    )

  @Test def `trait` =
    check(
      """trait M@@yTrait
        |""".stripMargin,
      "trait MyTrait: MyTrait".hover
    )

  @Test def `class` =
    check(
      """trait M@@yClass
        |""".stripMargin,
      "trait MyClass: MyClass".hover
    )

  @Test def `package` =
    check(
      """package b.p@@kg
        |object Main
        |""".stripMargin,
      "package b.pkg".hover
    )

  @Test def `pat-bind` =
    check(
      """
        |object Main {
        |  List(1) match {
        |    case h@@ead :: _ =>
        |  }
        |}
        |""".stripMargin,
      "val head: Int".hover
    )

  @Test def `pat-bind2` =
    check(
      """
        |object Main {
        |  Option(1) match {
        |    case Some(val@@ue) =>
        |  }
        |}
        |""".stripMargin,
      "val value: Int".hover
    )

  @Test def `val-int-literal` =
    check(
      """object a {
        |  <<val @@x : 1 = 1>>
        |}
        |""".stripMargin,
      """|Int
         |val x: 1""".stripMargin.hover
    )

  @Test def `val-int-literal-union` =
    check(
      """object a {
        |  <<val @@x : 1 | 2 = 1>>
        |}
        |""".stripMargin,
      "val x: 1 | 2".hover
    )

  @Test def `dealias-appliedtype-params` =
    check(
      """|trait Base {
         |  type T
         |  def f(t: T): Option[T] = Some(t)
         |}
         |
         |object Derived extends Base {
         |  override type T = Int
         |}
         |object O {
         |  <<val @@x = Derived.f(42)>>
         |}
         |""".stripMargin,
      """|```scala
         |val x: Option[Int]
         |```
         |""".stripMargin.hover
    )

  @Test def `trailing-whitespace` =
    check(
      """|object A {
         |
         |  <<val foo@@ = 123>>
         |}
         |""".stripMargin,
      "val foo: Int".hover
    )

  @Test def `i22335` =
    check(
      """|def fromInt[T: Numeric as n@@um](t: Int): T = num.fromInt(t)
         |""".stripMargin,
      """|num: Numeric[T]
         |""".stripMargin.hover
    )

  @Test def `i22335-2` =
    check(
      """|def showMax[X : {Numeric as nu@@m, Ordered as ord}](x: X, y: X): String = ???
         |""".stripMargin,
      """|num: Numeric[X]
          |""".stripMargin.hover
    )

  @Test def `i22335-3` =
    check(
      """|def showMax[X : {Nu@@meric as num, Ordered as ord}](x: X, y: X): String = ???
         |""".stripMargin,
      """|type Numeric: Numeric
         |""".stripMargin.hover
    )

  @Test def i7256 =
    check(
      """|object Test:
         |  def methodA: Unit = ???
         |export Test.me@@thodA
         |""".stripMargin,
      """|**Expression type**:
         |```scala
         |=> Unit
         |```
         |**Symbol signature**:
         |```scala
         |def methodA: Unit
         |```
         |""".stripMargin
    )

  @Test def `annotation` =
    check(
      """|
         |@ma@@in
         |def example() =
         |    println("test")
         |""".stripMargin,
      """|```scala
         |def this(): main
         |```""".stripMargin.hover
    )

  @Test def `annotation-2` =
    check(
      """|
          |@ma@@in
          |def example() =
          |    List("test")
          |""".stripMargin,
      """|```scala
          |def this(): main
          |```""".stripMargin.hover
    )

  @Test def `annotation-3` =
    check(
      """|
          |@ma@@in
          |def example() =
          |    Array("test")
          |""".stripMargin,
      """|```scala
          |def this(): main
          |```""".stripMargin.hover
    )

  @Test def `annotation-4` =
    check(
      """|
          |@ma@@in
          |def example() =
          |    Array(1, 2)
          |""".stripMargin,
      """|```scala
          |def this(): main
          |```""".stripMargin.hover
    )

  @Test def `opaque-type-method` =
    check(
      """|object History {
           |  opaque type Builder[A] = String
           |  def <<bui@@ld>>(b: Builder[Unit]): Int = ???
           |}
           |""".stripMargin,
      """|def build(b: Builder[Unit]): Int
           |""".stripMargin.hover
    )

  @Test def `backticked` =
    check(
      """|object A {
         |  <<val `foo @@ bar` = 123>>
         |}
         |""".stripMargin,
      "val `foo  bar`: Int".hover
    )
