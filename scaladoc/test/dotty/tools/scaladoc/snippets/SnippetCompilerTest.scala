package dotty.tools.scaladoc
package snippets

import org.junit.Test
import org.junit.Assert._
import dotty.tools.io.{AbstractFile, VirtualDirectory}

class SnippetCompilerTest {
  val compiler = SnippetCompiler(
    Seq(SnippetCompilerSetting(testContext.settings.usejavacp, true))
  )
  def wrapFn: String => WrappedSnippet = (str: String) => WrappedSnippet(
    str,
    Some("test"),
    Nil,
    Nil,
    0,
    0
  )

  def runTest(str: String) = compiler.compile(wrapFn(str), SnippetCompilerArg(SCFlags.Compile), dotty.tools.dotc.util.SourceFile.virtual("test", str))

  private def assertSuccessfulCompilation(res: SnippetCompilationResult): Unit = res match {
    case r @ SnippetCompilationResult(_, isSuccessful, _, messages) => assert(isSuccessful, r.messages.map(_.message).mkString("\n"))
  }

  private def assertFailedCompilation(res: SnippetCompilationResult): Unit = res match {
    case r @ SnippetCompilationResult(_, isSuccessful, _, messages) => assert(!isSuccessful, r.messages.map(_.message).mkString("\n"))
  }

  def assertSuccessfulCompilation(str: String): Unit = assertSuccessfulCompilation(runTest(str))

  def assertFailedCompilation(str: String): Unit = assertFailedCompilation(runTest(str))

  def assertMessageLevelPresent(str: String, level: MessageLevel): Unit = assertMessageLevelPresent(runTest(str), level)

  def assertMessageLevelPresent(res: SnippetCompilationResult, level: MessageLevel): Unit = res match {
    case r @ SnippetCompilationResult(_, isSuccessful, _, messages) => assertTrue(
      s"Expected message with level: ${level.text}. Got result ${r.messages.map(_.message).mkString("\n")}",
      messages.exists(_.level == level)
    )
  }


  @Test
  def snippetCompilerTest: Unit = {
    val simpleCorrectSnippet = s"""
      |class A:
      |  val b: String = "asd"
      |""".stripMargin

    val simpleIncorrectSnippet = s"""
      |class A:
      |  val b: String
      |""".stripMargin
    val warningSnippet = s"""
      |class A:
      |  val a: Int = try {
      |    5
      |  }
      |""".stripMargin
    assertSuccessfulCompilation(simpleCorrectSnippet)
    assertFailedCompilation(simpleIncorrectSnippet)
    assertMessageLevelPresent(simpleIncorrectSnippet, MessageLevel.Error)
    assertMessageLevelPresent(warningSnippet, MessageLevel.Warning)
    //No test for Info
  }
}