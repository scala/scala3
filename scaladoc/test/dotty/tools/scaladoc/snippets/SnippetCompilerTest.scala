package dotty.tools.scaladoc
package snippets

import org.junit.Test
import org.junit.Assert._
import dotty.tools.io.{AbstractFile, VirtualDirectory}

class SnippetCompilerTest {
  val compiler = SnippetCompiler()
  def runTest(str: String) = compiler.compile(str)

  def runTest(str: List[String]) = compiler.compile(str)

  private def assertSuccessfulCompilation(res: SnippetCompilationResult): Unit = res match {
    case SnippetCompilationResult(Some(target), _) => assert(true)
    case r @ SnippetCompilationResult(None, _) => assert(false, r.getSummary)
  }

  private def assertFailedCompilation(res: SnippetCompilationResult): Unit = res match {
    case SnippetCompilationResult(Some(target), _) => assert(false, "Expected compilation failure")
    case r @ SnippetCompilationResult(None, _) => assert(true)
  }

  def assertSuccessfulCompilation(str: String): Unit = assertSuccessfulCompilation(runTest(str))

  def assertFailedCompilation(str: String): Unit = assertFailedCompilation(runTest(str))

  def assertSuccessfulCompilation(str: List[String]): Unit = assertSuccessfulCompilation(runTest(str))

  def assertFailedCompilation(str: List[String]): Unit = assertFailedCompilation(runTest(str))


  @Test
  def snippetCompilerTest: Unit = {
    val simpleCorrectSnippet = s"""
      |package asd
      |class A:
      |  val b: String = "asd"
      |""".stripMargin

    val simpleIncorrectSnippet = s"""
      |package asd
      |class A:
      |  val b: String
      |""".stripMargin
    assertSuccessfulCompilation(simpleCorrectSnippet)
    assertFailedCompilation(simpleIncorrectSnippet)
    assertFailedCompilation(List(simpleCorrectSnippet, simpleCorrectSnippet))
  }
}