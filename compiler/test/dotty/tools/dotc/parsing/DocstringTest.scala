package dotty.tools
package dotc
package parsing

import ast.Trees._
import core.Contexts.Context

import dotty.uoption._

trait DocstringTest extends DottyTest {
  ctx = ctx.fresh.setSetting(ctx.settings.YkeepComments, true)

  def checkDocString(actual: UOption[String], expected: String): Unit = actual match {
    case USome(str) =>
      assert(str == expected, s"""Docstring: "$str" didn't match expected "$expected"""")
    case UNone =>
      assert(false, s"""No docstring found, expected: "$expected"""")
  }

  def expectNoDocString(doc: UOption[String]): Unit =
    doc.fold(()) { d => assert(false, s"""Expected not to find a docstring, but found: "$d"""") }

  def defaultAssertion: PartialFunction[Any, Unit] = {
    case t: Tree[Untyped] =>
      assert(false, s"Couldn't match resulting AST to expected AST in: ${t.show}")
    case x =>
      assert(false, s"Couldn't match resulting AST to expected AST in: $x")
  }

  def checkFrontend(source: String)(docAssert: PartialFunction[Tree[Untyped], Unit]): Unit = {
    checkCompile("frontend", source) { (_, ctx) =>
      implicit val c: Context = ctx
      (docAssert orElse defaultAssertion)(ctx.compilationUnit.untpdTree)
    }
  }
}
