package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.Test
import org.junit.Ignore
import java.nio.file.Path

class SignatureHelpInterleavingSuite extends BaseSignatureHelpSuite:

  @Test def `proper-position-1` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[@@Int](1)[String]("1")
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |     ^
        |""".stripMargin
    )


  @Test def `proper-position-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](@@1)[String]("1")
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `proper-position-3` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](1)[@@String]("1")
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `proper-position-4` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](1)[String](@@"1")
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `not-fully-applied-1` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[@@Int]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |     ^
        |""".stripMargin
    )


  @Test def `not-fully-applied-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](@@1)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `not-fully-applied-3` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](1)[@@String]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `error` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int][@@String]
      """.stripMargin,
      """|apply(v1: Int): Any => (Int, Any)
         |      ^^^^^^^
         |""".stripMargin
    )

  @Test def `inferred-type-param-1` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair(1@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `inferred-type-param-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair(1)[String@@]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `inferred-type-param-3` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair(1)[String]("1"@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `inferred-type-param-4` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair(1)("1"@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `empty-current-param-check` =
    check(
      """
        |object Test:
        |  def pair[A](a: A): A = ???
        |  pair[@@]
      """.stripMargin,
      """
        |pair[A](a: A): A
        |     ^
        |""".stripMargin
    )

  @Test def `empty-current-param-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `empty-current-param-3` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](1)[@@]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `empty-current-param-4` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[Int](1)[String](@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `empty-current-inferred-1` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair(@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `empty-current-inferred-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair(1)(@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `empty-previous-1` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[](@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `empty-previous-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[]()[@@]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `empty-previous-3` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[]()[](@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `empty-previous-4` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[]()[](11@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `empty-previous-5` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[](5@@1)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |        ^^^^
        |""".stripMargin
    )

  @Test def `empty-previous-6` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[String]()[@@]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `empty-previous-7` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[String]()[Int](@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `error-previous-1` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[String](52)[@@]
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |              ^
        |""".stripMargin
    )

  @Test def `error-previous-2` =
    check(
      """
        |object Test:
        |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
        |  pair[String](52)[Int](""@@)
      """.stripMargin,
      """
        |pair[A](a: A)[B](b: B): (A, B)
        |                 ^^^^
        |""".stripMargin
    )

  @Test def `complex-1` =
    check(
      """
        |object Test:
        |  def foo[A](using a: A)(b: List[A])[C <: a.type, D](cd: (C, D))[E]: Foo[A, B, C, D, E]
        |  foo[Int](using 1)(List(1, 2, 3))(@@)
      """.stripMargin,
      """
        |foo[A](using a: A)(b: List[A])[C <: a.type, D](cd: (C, D))[E]: Foo[A, B, C, D, E]
        |                                               ^^^^^^^^^^
        |""".stripMargin
    )

  @Test def `complex-2` =
    check(
      """
        |object Test:
        |  def foo[A](using a: A)(b: List[A])[C <: a.type, D](cd: (C, D))[E]: Foo[A, B, C, D, E]
        |  foo[Int](using 1)(List(1, 2, 3))((1, 2))[@@]
      """.stripMargin,
      """
        |foo[A](using a: A)(b: List[A])[C <: a.type, D](cd: (C, D))[E]: Foo[A, B, C, D, E]
        |                                                           ^
        |""".stripMargin
    )

  @Ignore("""Clause interleaving is still experimental. It lifts this tree into series of anonymous functions, which all have the same span.
             It requires further investigation to determine whether this is a bug in the compiler.""")
  @Test def `clause-interleaving-empty` =
    check(
      """|object M:
         |  def test[X](x: X)[Y](y: Y): (X, Y)= ???
         |  test[@@]
         |""".stripMargin,
      """|test[X](x: X)[Y](y: Y): (X, Y)
         |     ^
         |""".stripMargin
    )

  @Ignore("""Clause interleaving is still experimental. It lifts this tree into series of anonymous functions, which all have the same span.
             It requires further investigation to determine whether this is a bug in the compiler.""")
  @Test def `more-interleaved-params-1` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[@@]
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |     ^
         |""".stripMargin
    )

  @Test def `more-interleaved-params-2` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](@@)
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |        ^^^^
         |""".stripMargin
    )

  @Ignore("""Clause interleaving is still experimental. It lifts this tree into series of anonymous functions, which all have the same span.
             It requires further investigation to determine whether this is a bug in the compiler.""")
  @Test def `more-interleaved-params-3` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)[@@]
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |              ^
         |""".stripMargin
    )

  @Test def `more-interleaved-params-4` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)[String](@@)
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |                 ^^^^
         |""".stripMargin
    )

  @Ignore("""Clause interleaving is still experimental. It lifts this tree into series of anonymous functions, which all have the same span.
             It requires further investigation to determine whether this is a bug in the compiler.""")
  @Test def `more-interleaved-params-5` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)[String]("1")[@@]
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |                       ^
         |""".stripMargin
    )

  @Test def `more-interleaved-params-6` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)[String]("1")[Int](@@)
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |                          ^^^^
         |""".stripMargin
    )

  @Test def `more-interleaved-params-7` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)[String]("1")[Int](2)[@@]
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |                                ^
         |""".stripMargin
    )

  @Test def `more-interleaved-params-8` =
    check(
      """|object M:
         |  def test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)[String]("1")[Int](2)[String](@@)
         |""".stripMargin,
      """|test[A](a: A)[B](b: B)[C](c: C)[D](d: D): Int
         |                                   ^^^^
         |""".stripMargin
    )

  @Test def `interleaving-with-implicit` =
    check(
      """|object M:
         |  def test[A](a: A)(using Int)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)(using 5)[String]("1")[Int](@@)
         |""".stripMargin,
      """|test[A](a: A)(using Int)[B](b: B)[C](c: C)[D](d: D): Int
         |                                     ^^^^
         |""".stripMargin
    )

  @Test def `interleaving-with-implicit-recovery` =
    check(
      """|object M:
         |  def test[A](a: A)(using Int)[B](b: B)[C](c: C)[D](d: D): Int = ???
         |  test[Int](1)(5)[String]("1")[Int](@@)
         |""".stripMargin,
      """|test[A](a: A)(using Int)[B](b: B)[C](c: C)[D](d: D): Int
         |                                     ^^^^
         |""".stripMargin
    )

