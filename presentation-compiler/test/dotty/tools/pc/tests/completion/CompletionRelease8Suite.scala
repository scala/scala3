package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test
import java.nio.file.Path

class CompletionRelease8Suite extends BaseCompletionSuite:

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    "-release:8" +: super.scalacOptions(classpath)

  @Test def noJvm11Symbols =
    check(
      """
        |object A {
        |  "".repea@@
        |}""".stripMargin,
      """replaceAll(x$0: String, x$1: String): String
        |prependedAll[B >: A](prefix: IterableOnce[B]): IndexedSeq[B]
        |prependedAll(prefix: String): String
        |prependedAll[B >: Char](prefix: IterableOnce[B]): IndexedSeq[B]
        |replaceAllLiterally(literal: String, replacement: String): String
        |""".stripMargin
    )
