package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test

class SingletonCompletionsSuite extends BaseCompletionSuite {

  @Test def `basic` =
    check(
      """|val k: 1 = @@
         |""".stripMargin,
      "1: 1",
      topLines = Some(1)
    )

  @Test def `literal` =
    check(
      """|val k: 1 = 1@@
         |""".stripMargin,
      "1: 1",
      topLines = Some(1)
    )

  @Test def `string` =
    check(
      """|val k: "aaa" = "@@"
         |""".stripMargin,
      """|"aaa": "aaa"
         |""".stripMargin
    )

  @Test def `string-edit` =
    checkEdit(
      """|val k: "aaa" = "@@"
         |""".stripMargin,
      """|val k: "aaa" = "aaa"
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `string-edit-2` =
    checkEdit(
      """|val k: "aaa" = @@ //something
         |""".stripMargin,
      """|val k: "aaa" = "aaa" //something
         |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `union` =
    check(
      """|val k: "aaa" | "bbb" = "@@"
         |""".stripMargin,
      """|"aaa": "aaa" | "bbb"
         |"bbb": "aaa" | "bbb"
         |""".stripMargin
    )

  @Test def `type-alias-union` =
    check(
      """|type Color = "red" | "green" | "blue"
         |val c: Color = "r@@"
         |""".stripMargin,
      """|"red": Color
         |""".stripMargin
    )

  @Test def `param` =
    check(
      """|type Color = "red" | "green" | "blue"
         |def paint(c: Color) = ???
         |val _ = paint(@@)
         |""".stripMargin,
      """|"red": Color
         |"green": Color
         |"blue": Color
         |c = : Color
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `with-block` =
    check(
      """|type Color = "red" | "green" | "blue"
         |def c: Color = {
         |  "r@@"
         |}
         |""".stripMargin,
      """|"red": Color
         |""".stripMargin
    )

  @Test def `if-statement` =
    check(
      """|type Color = "red" | "green" | "blue"
         |def c(shouldBeBlue: Boolean): Color = {
         |  if(shouldBeBlue) "b@@"
         |  else "red"
         |}
         |""".stripMargin,
      """|"blue": Color
         |""".stripMargin
    )

  @Test def `if-statement-2` =
    check(
      """|type Color = "red" | "green" | "blue"
         |def c(shouldBeBlue: Boolean): Color = {
         |  if(shouldBeBlue) {
         |    println("is blue")
         |    "b@@"
         |  } else "red"
         |}
         |""".stripMargin,
      """|"blue": Color
         |""".stripMargin
    )

  @Test def `if-statement-3` =
    check(
      """|type Color = "red" | "green" | "blue"
         |def c(shouldBeBlue: Boolean): Color = {
         |  if(shouldBeBlue) {
         |    "b@@"
         |    println("is blue")
         |    "blue"
         |  } else "red"
         |}
         |""".stripMargin,
      """""".stripMargin
    )

  @Test def `middle-of-a-block` =
    check(
      """|type Color = "red" | "green" | "blue"
         |def c: Color = {
         |  "r@@"
         |  ???
         |}
         |""".stripMargin,
      ""
    )

  @Test def overloaded =
    check(
      """|
         |type Color = "red" | "green" | "blue"
         |def foo(i: Int) = ???
         |def foo(c: Color) = ???
         |
         |def c = foo(@@)
         |""".stripMargin,
      """|c = : Color
         |i = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `and-type` =
    check(
      """|type Color = "red" | "green" | "blue" | "black"
         |type FordColor = Color & "black"
         |val i: FordColor = "@@"
         |""".stripMargin,
      """|"black": FordColor
         |""".stripMargin
    )

  @Test def list =
    check(
      """|type Color = "red" | "green" | "blue"
         |val i: List[Color] = List("@@")
         |""".stripMargin,
      """|"red": "red" | "green" | "blue"
         |"green": "red" | "green" | "blue"
         |"blue": "red" | "green" | "blue"
         |""".stripMargin
    )

  @Test def option =
    check(
      """|type Color = "red" | "green" | "blue"
         |val i: Option[Color] = Some("@@")
         |""".stripMargin,
      """|"red": "red" | "green" | "blue"
         |"green": "red" | "green" | "blue"
         |"blue": "red" | "green" | "blue"
         |""".stripMargin
    )

  @Test def map =
    check(
      """|type Color = "red" | "green" | "blue"
         |val i: Option[Int] = Some(1)
         |val g: Option[Color] = i.map { _ => "@@" }
         |""".stripMargin,
      """|"red": "red" | "green" | "blue"
         |"green": "red" | "green" | "blue"
         |"blue": "red" | "green" | "blue"
         |""".stripMargin
    )

  @Test def `some-for-comp` =
    check(
      """|type Color = "red" | "green" | "blue"
         |val i: Option[Int] = Some(1)
         |val g: Option[Color] =
         |  for
         |    _ <- i
         |  yield "@@"
         |""".stripMargin,
      """|"red": "red" | "green" | "blue"
         |"green": "red" | "green" | "blue"
         |"blue": "red" | "green" | "blue"
         |""".stripMargin
    )

  @Test def `some-for-comp-1` =
    check(
      """|type Color = "red" | "green" | "blue"
         |val i: Option[Int] = Some(1)
         |val g: Option[Color] =
         |  for
         |    _ <- i
         |    _ <- i
         |    if i > 2
         |  yield "@@"
         |""".stripMargin,
      """|"red": "red" | "green" | "blue"
         |"green": "red" | "green" | "blue"
         |"blue": "red" | "green" | "blue"
         |""".stripMargin
    )

  @Test def lambda =
    check(
      """|def m =
         |  val j = (f: "foo") => 1
         |  j("f@@")
         |""".stripMargin,
      """|"foo": "foo"
         |""".stripMargin
    )

  @Test def `match-case-result` =
    check(
      """|val h: "foo" =
         |  1 match
         |    case _ => "@@"
         |""".stripMargin,
      """|"foo": "foo"
         |""".stripMargin
    )

  @Test def `dont-show-on-select` =
    check(
      """|val f: "foo" = List(1,2,3).@@
         |""".stripMargin,
      "",
      filter = _ == "\"foo\": \"foo\""
    )

  @Test def `match-case` =
    check(
      """|def h(foo: "foo") =
          |  foo match
          |    case "@@" =>
          |""".stripMargin,
      """|"foo": "foo"
          |""".stripMargin
    )

  @Test def `match-case2` =
    check(
      """|def h =
         |  ("foo" : "foo") match
         |    case "@@" =>
         |""".stripMargin,
      """|"foo": "foo"
         |""".stripMargin
    )

  @Test def `named-args` =
    check(
      """|def h(foo: "foo") = ???
         |def k = h(foo = "@@")
         |""".stripMargin,
      """|"foo": "foo"
         |""".stripMargin
    )

  @Test def `map-type` =
    check(
      """|def m = Map["foo", Int]("@@")
         |""".stripMargin,
      """|"foo": "foo"
         |""".stripMargin
    )

  @Test def `type-apply` =
    check(
      """|class Consumer[A]:
         |  def eat(a: A) = ()
         |
         |def test =
         |  Consumer[7].eat(@@)
         |""".stripMargin,
      "7: 7",
      topLines = Some(1)
    )

  @Test def `type-apply-2` =
    check(
      """|class Consumer[A]:
         |  def eat(a: A) = ()
         |
         |object Consumer7 extends Consumer[7]
         |
         |def test =
         |  Consumer7.eat(@@)
         |""".stripMargin,
      "7: 7",
      topLines = Some(1)
    )
}
