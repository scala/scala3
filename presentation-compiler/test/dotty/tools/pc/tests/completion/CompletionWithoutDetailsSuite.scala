package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.pc.PresentationCompilerConfig

import org.junit.Test

class CompletionWithoutDetailsSuite extends BaseCompletionSuite:

  override def config: PresentationCompilerConfigImpl =
    super.config.copy(
      isDetailIncludedInLabel = false
    )

  @Test def `scope` =
    check(
      """
        |object A {
        |  Lis@@
        |}""".stripMargin,
      """|List
         |List
         |List
         |List
         |ListUI
         |""".stripMargin,
      includeDetail = false,
      topLines = Some(5)
    )

  @Test def `scope-detail` =
    check(
      """
        |object A {
        |  Lis@@
        |}""".stripMargin,
      """|List[A](elems: A*): List[A]
         |List scala.collection.immutable
         |List java.awt
         |List java.util
         |ListUI javax.swing.plaf
        |""".stripMargin,
      includeDetail = true,
      topLines = Some(5)
    )

  @Test def member =
    check(
      """
        |object A {
        |  List.emp@@
        |}""".stripMargin,
      """
        |empty
        |""".stripMargin,
      includeDetail = false
    )

  @Test def extension =
    check(
      """
        |object A {
        |  "".stripSu@@
        |}""".stripMargin,
      """|stripSuffix
        |""".stripMargin,
      includeDetail = false
    )

  @Test def tparam =
    check(
      """
        |class Foo[A] {
        |  def identity[B >: A](a: B): B = a
        |}
        |object Foo {
        |  new Foo[Int].ident@@
        |}""".stripMargin,
      """|identity
        |""".stripMargin,
      includeDetail = false
    )

  @Test def tparam1 =
    check(
      """
        |class Foo[A] {
        |  def identity(a: A): A = a
        |}
        |object Foo {
        |  new Foo[Int].ident@@
        |}""".stripMargin,
      """|identity
         |""".stripMargin,
      includeDetail = false
    )

  @Test def tparam2 =
    check(
      """
        |object A {
        |  Map.empty[Int, String].getOrEl@@
        |}
        |""".stripMargin,
      """|getOrElse
         |""".stripMargin,
      includeDetail = false
    )

  @Test def pkg =
    check(
      """
        |import scala.collection.conc@@
        |""".stripMargin,
      """|concurrent
         |""".stripMargin,
      includeDetail = false
    )