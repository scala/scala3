package dotty.tools.dotc.reporting

import dotty.tools.DottyTest
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourceFile

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.runtime.Scala3RunTime.assertFailed

import org.junit.Assert._
import org.junit.Test

/** This is a test suite that is meant to test the actions attached to the
  * diagnostic for a given code snippet.
  */
class CodeActionTest extends DottyTest:

  @Test def convertToFunctionValue =
    checkCodeAction(
      """|object Test:
         |  def x: Int = 3
         |  val test = x _
         |""".stripMargin,
      "Rewrite to function value",
      """|object Test:
         |  def x: Int = 3
         |  val test = (() => x)
         |""".stripMargin
      )

  @Test def insertBracesForEmptyArgument =
    checkCodeAction(
      """|object Test:
         |  def foo(): Unit = ()
         |  val x = foo
         |""".stripMargin,
      "Insert ()",
      """|object Test:
         |  def foo(): Unit = ()
         |  val x = foo()
         |""".stripMargin

      )

  @Test def removeRepeatModifier =
    checkCodeAction(
      """|final final class Test
         |""".stripMargin,
      """Remove repeated modifier: "final"""",
      // TODO look into trying to remove the extra space that is left behind
      """|final  class Test
         |""".stripMargin

      )

  // Make sure we're not using the default reporter, which is the ConsoleReporter,
  // meaning they will get reported in the test run and that's it.
  private def newContext =
    val rep = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    initialCtx.setReporter(rep).withoutColors

  private def checkCodeAction(code: String, title: String, expected: String) =
    ctx = newContext
    val source = SourceFile.virtual("test", code).content
    val runCtx = checkCompile("typer", code) { (_, _) => () }
    val diagnostics = runCtx.reporter.removeBufferedMessages
    assertEquals(1, diagnostics.size)

    val diagnostic = diagnostics.head
    val actions = diagnostic.msg.actions.asScala.toList
    assertEquals(1, actions.size)

    // TODO account for more than 1 action
    val action = actions.head
    assertEquals(action.title, title)
    val patches = action.patches.asScala.toList
    if patches.nonEmpty then
      patches.reduceLeft: (p1, p2) =>
        assert(p1.srcPos.span.end <= p2.srcPos.span.start, s"overlapping patches $p1 and $p2")
        p2
    else assertFailed("Expected a patch attatched to this action, but it was empty")

    val delta = patches
      .map: patch =>
        patch.replacement.length - (patch.srcPos.end - patch.srcPos.start)
      .sum

    val result = new Array[Char](source.length + delta)

    @tailrec def loop(ps: List[ActionPatch], inIdx: Int, outIdx: Int): Unit =
      def copy(upTo: Int): Int =
        val untouched = upTo - inIdx
        System.arraycopy(source, inIdx, result, outIdx, untouched)
        outIdx + untouched

      ps match
        case patch @ ActionPatch(srcPos, replacement) :: ps1 =>
          val outNew = copy(srcPos.start)
          replacement.copyToArray(result, outNew)
          loop(ps1, srcPos.end, outNew + replacement.length)
        case Nil =>
          val outNew = copy(source.length)
          assert(outNew == result.length, s"$outNew != ${result.length}")

    loop(patches, 0, 0)
    assertEquals(expected, result.mkString)
