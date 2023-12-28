package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.Test
import java.nio.file.Path

class SignatureHelpInterleavingSuite extends BaseSignatureHelpSuite:

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    List("-language:experimental.clauseInterleaving")

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
      ""
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

  // Bug in compiler: TypeApply fun has incorrect span for clause interleaving
  // @Test def `empty-current-param-1` =
  //   check(
  //     """
  //       |object Test:
  //       |  def pair[A](a: A)[B](b: B): (A, B) = (a, b)
  //       |  pair[@@]
  //     """.stripMargin,
  //     """
  //       |pair[A](a: A)[B](b: B): (A, B)
  //       |     ^
  //       |""".stripMargin
  //   )

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
