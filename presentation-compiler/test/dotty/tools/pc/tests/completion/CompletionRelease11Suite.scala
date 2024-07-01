package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test
import java.nio.file.Path

class CompletionRelease11Suite extends BaseCompletionSuite:

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    "-release:11" +: super.scalacOptions(classpath)

  @Test def java11Symbols =
    check(
      """
        |object A {
        |  "".repea@@
        |}""".stripMargin,
      """repeat(x$0: Int): String
        |replaceAll(x$0: String, x$1: String): String
        |prependedAll[B >: A](prefix: IterableOnce[B]): IndexedSeq[B]
        |prependedAll(prefix: String): String
        |prependedAll[B >: Char](prefix: IterableOnce[B]): IndexedSeq[B]
        |replaceAllLiterally(literal: String, replacement: String): String
        |""".stripMargin
    )
