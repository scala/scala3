package dotty.tools
package dotc
package parsing

import ast.Trees._
import core.Contexts.Context

trait DocstringTest extends DottyTest {

  def checkDocString(actual: Option[String], expected: String): Unit = actual match {
    case Some(str) =>
      assert(str == expected, s"""Docstring: "$str" didn't match expected "$expected"""")
    case None =>
      assert(false, s"""No docstring found, expected: "$expected"""")
  }

  def expectNoDocString(doc: Option[String]): Unit =
    doc.fold(()) { d => assert(false, s"""Expected not to find a docstring, but found: "$d"""") }

  def defaultAssertion: PartialFunction[Any, Unit] = {
    case t: Tree[Untyped] =>
      assert(false, s"Couldn't match resulting AST to expected AST in: ${t.show}")
    case x =>
      assert(false, s"Couldn't match resulting AST to expected AST in: $x")
  }

  def checkFrontend(source: String)(docAssert: PartialFunction[Tree[Untyped], Unit]): Unit = {
    checkCompile("typer", source) { (_, ctx) =>
      implicit val c: Context = ctx
      (docAssert `orElse` defaultAssertion)(ctx.compilationUnit.untpdTree)
    }
  }
}
