package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.Test
import org.junit.Before
import java.nio.file.Path
import dotty.tools.pc.utils.JRE

class CompletionRelease8Suite extends BaseCompletionSuite:

  override protected def scalacOptions(classpath: Seq[Path]): Seq[String] =
    "-release:8" +: super.scalacOptions(classpath)

  @Before
  def beforeMethod(): Unit =
    org.junit.Assume.assumeTrue(JRE.getJavaMajorVersion >= 8)

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
