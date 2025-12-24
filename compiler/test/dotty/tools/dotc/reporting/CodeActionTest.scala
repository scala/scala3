package dotty.tools.dotc.reporting

import dotty.tools.DottyTest
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.dotc.rewrites.Rewrites.ActionPatch
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.core.Contexts._

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

  @Test def addUsingClause =
    checkCodeAction(
      """|object Test:
         |  def foo(implicit a: Int) = a
         |  foo(123)
         |""".stripMargin,
      "Add `using` clause",
      """|object Test:
         |  def foo(implicit a: Int) = a
         |  foo(using 123)
         |""".stripMargin
      )

  @Test def insertMissingCases =
    checkCodeAction(
      code =
        """|enum Tree:
           |  case Node(l: Tree, r: Tree)
           |  case Leaf(v: String)
           |
           |object Test:
           |  def foo(tree: Tree) = tree match {
           |    case Tree.Node(_, _) => ???
           |  }
           |""".stripMargin,
         title = "Insert missing cases (1)",
      expected =
        """|enum Tree:
           |  case Node(l: Tree, r: Tree)
           |  case Leaf(v: String)
           |
           |object Test:
           |  def foo(tree: Tree) = tree match {
           |    case Tree.Node(_, _) => ???
           |    case Tree.Leaf(_) => ???
           |  }
           |""".stripMargin,
         afterPhase = "patternMatcher"
      )

  @Test def insertMissingCasesForUnionStringType =
    checkCodeAction(
      code =
        """object Test:
           |  def foo(text: "Alice" | "Bob") = text match {
           |    case "Alice" => ???
           |  }
           |""".stripMargin,
         title = "Insert missing cases (1)",
      expected =
        """object Test:
           |  def foo(text: "Alice" | "Bob") = text match {
           |    case "Alice" => ???
           |    case "Bob" => ???
           |  }
           |""".stripMargin,
         afterPhase = "patternMatcher"
      )

  @Test def insertMissingCasesForUnionIntType =
    checkCodeAction(
      code =
        """object Test:
           |  def foo(text: 1 | 2) = text match {
           |    case 2 => ???
           |  }
           |""".stripMargin,
         title = "Insert missing cases (1)",
      expected =
        """object Test:
           |  def foo(text: 1 | 2) = text match {
           |    case 2 => ???
           |    case 1 => ???
           |  }
           |""".stripMargin,
         afterPhase = "patternMatcher"
      )

  @Test def insertMissingCasesUsingBracelessSyntax =
    checkCodeAction(
      code =
        """object Test:
           |  def foo(text: 1 | 2) = text match
           |    case 2 => ???
           |""".stripMargin,
         title = "Insert missing cases (1)",
      expected =
        """object Test:
           |  def foo(text: 1 | 2) = text match
           |    case 2 => ???
           |    case 1 => ???
           |""".stripMargin,
         afterPhase = "patternMatcher"
      )

  @Test def removeNN =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """|val s: String|Null = "foo".nn
           |""".stripMargin,
         title = "Remove unnecessary .nn",
      expected =
        """|val s: String|Null = "foo"
           |""".stripMargin,
      ctxx = ctxx
      )


  @Test def removeNN2 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """val s: String|Null = null.nn
           |""".stripMargin,
         title = "Remove unnecessary .nn",
      expected =
        """val s: String|Null = null
           |""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN1 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """val s: String|Null = ???
          | val t: String = s""".stripMargin,
        title = "Add .nn",
      expected =
        """val s: String|Null = ???
          | val t: String = s.nn""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN2 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """implicit class infixOpTest(val s1: String) extends AnyVal {
          |  def q(s2: String): String | Null = null
          |}
          | val s: String = ???
          | val t: String = s q s""".stripMargin,
        title = "Add .nn",
      expected =
        """implicit class infixOpTest(val s1: String) extends AnyVal {
          |  def q(s2: String): String | Null = null
          |}
          | val s: String = ???
          | val t: String = (s q s).nn""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN3 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """implicit class infixOpTest(val s1: String) extends AnyVal {
          |  def q(s2: String, s3: String): String | Null = null
          |}
          | val s: String = ???
          | val t: String = s q (s, s)""".stripMargin,
        title = "Add .nn",
      expected =
        """implicit class infixOpTest(val s1: String) extends AnyVal {
          |  def q(s2: String, s3: String): String | Null = null
          |}
          | val s: String = ???
          | val t: String = (s q (s, s)).nn""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN4 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """implicit class infixOpTest(val s1: String) extends AnyVal {
          |  def q(s2: String, s3: String): String | Null = null
          |}
          | val s: String = ???
          | val t: String = s.q(s, s)""".stripMargin,
        title = "Add .nn",
      expected =
        """implicit class infixOpTest(val s1: String) extends AnyVal {
          |  def q(s2: String, s3: String): String | Null = null
          |}
          | val s: String = ???
          | val t: String = s.q(s, s).nn""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN5 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """val s: String | Null = ???
          |val t: String = s match {
          | case _: String => "foo"
          | case _ => s
          |}""".stripMargin,
        title = "Add .nn",
      expected =
        """val s: String | Null = ???
          |val t: String = s match {
          | case _: String => "foo"
          | case _ => s.nn
          |}""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN6 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """val s: String | Null = ???
          |val t: String = if (s != null) "foo" else s""".stripMargin,
        title = "Add .nn",
      expected =
        """val s: String | Null = ???
          |val t: String = if (s != null) "foo" else s.nn""".stripMargin,
      ctxx = ctxx
      )

  @Test def addNN7 =
    val ctxx = newContext
    ctxx.setSetting(ctxx.settings.YexplicitNulls, true)
    checkCodeAction(
      code =
        """given ctx: String | Null = null
          |def f(using c: String): String = c
          |val s: String = f(using ctx)""".stripMargin,
        title = "Add .nn",
      expected =
        """given ctx: String | Null = null
          |def f(using c: String): String = c
          |val s: String = f(using ctx.nn)""".stripMargin,
      ctxx = ctxx
      )

  // Make sure we're not using the default reporter, which is the ConsoleReporter,
  // meaning they will get reported in the test run and that's it.
  private def newContext =
    val rep = new StoreReporter(null) with UniqueMessagePositions with HideNonSensicalMessages
    initialCtx.setReporter(rep).withoutColors

  private def checkCodeAction(code: String, title: String, expected: String, afterPhase: String = "typer", ctxx: Context = newContext) =
    ctx = ctxx
    val source = SourceFile.virtual("test", code).content
    val runCtx = checkCompile(afterPhase, code) { (_, _) => () }
    val diagnostics = runCtx.reporter.removeBufferedMessages
    assertEquals("Expected exactly one diagnostic", 1, diagnostics.size)

    val diagnostic = diagnostics.head
    val actions = diagnostic.msg.actions.toList
    assertEquals("Expected exactly one action", 1, actions.size)

    // TODO account for more than 1 action
    val action = actions.head
    assertEquals(action.title, title)
    val patches = action.patches.toList
    if patches.nonEmpty then
      patches.reduceLeft: (p1, p2) =>
        assert(p1.srcPos.span.end <= p2.srcPos.span.start, s"overlapping patches $p1 and $p2")
        p2
    else assertFailed("Expected a patch attatched to this action, but it was empty")

    val result = patches.reverse.foldLeft(code): (newCode, patch)  =>
      import scala.language.unsafeNulls
      val start = newCode.substring(0, patch.srcPos.start)
      val ending = newCode.substring(patch.srcPos.end, newCode.length)
      start + patch.replacement + ending

    assertEquals(expected, result)
