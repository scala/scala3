package dotty.tools.scaladoc
package snippets

import org.junit.Test
import org.junit.Assert._

class SnippetCompilerTest {
  val compiler = SnippetCompiler(
    Seq(SnippetCompilerSetting(testContext.settings.Yusejavacp, true))
  )
  def wrapFn: String => WrappedSnippet = (str: String) => WrappedSnippet(
    str,
    Some("test"),
    0,
    0,
  )

  private def sourceFile(str: String) =
    dotty.tools.dotc.util.SourceFile.virtual("test", str)

  def runTest(
    str: String,
    arg: SnippetCompilerArg = SnippetCompilerArg(SCFlags.Compile)
  ) =
    compiler.compile(SnippetSource(str, 0), wrapFn(str), arg, sourceFile(str))

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

  // i16250: global language imports should be hoisted outside the snippet wrapper
  @Test
  def snippetGlobalLanguageImports: Unit = {
    assertSuccessfulCompilation(
      """|import language.experimental.pureFunctions
         |val x: Int -> Int = (a: Int) => a
         |""".stripMargin)
    assertSuccessfulCompilation(
      """|import language.experimental.captureChecking
         |def foo[C^](x: AnyRef^{C}): AnyRef^{x} = x
         |""".stripMargin)
    assertSuccessfulCompilation(
      """|import language.experimental.separationChecking
         |def foo[C^](x: AnyRef^{C}): AnyRef^{x} = x
         |""".stripMargin)
    assertSuccessfulCompilation(
      """|import language.experimental.safe
         |def foo[C^](x: AnyRef^{C}): AnyRef^{x} = x
         |""".stripMargin)
  }

  @Test
  def inlineExpectedErrorsCanDriveNegativeSnippets: Unit = {
    val snippet =
      """|val x = 1.missing // error
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertSuccessfulCompilation(result)
    assertMessageLevelPresent(result, MessageLevel.Error)
  }

  @Test
  def testedSnippetsWithoutDiagnosticsPass: Unit = {
    val snippet =
      """|val x = 1 + 1
         |""".stripMargin

    assertSuccessfulCompilation(runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true)))
  }

  @Test
  def testedSnippetsRequireErrorAnnotations: Unit = {
    val snippet =
      """|val x = 1.missing
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertFailedCompilation(result)
    assertEquals(2, result.messages.count(_.level == MessageLevel.Error))
    assertTrue(result.messages.exists(_.message.contains("No expected errors marked in snippet -- use // error")))
    assertTrue(result.messages.exists(_.message.contains("Unexpected error on line 1")))
  }

  @Test
  def testedSnippetsRequireWarningAnnotations: Unit = {
    val snippet =
      """|val a: Int = try { 5 }
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertFailedCompilation(result)
    assertEquals(2, result.messages.count(_.level == MessageLevel.Error))
    assertTrue(result.messages.exists(_.message.contains("No expected warnings marked in snippet -- use // warn")))
    assertTrue(result.messages.exists(_.message.contains("Unexpected warning on line 1")))
  }

  @Test
  def inlineExpectedErrorsWorkWithFailFlag: Unit = {
    val snippet =
      """|val x = 1.missing // error
         |""".stripMargin

    assertSuccessfulCompilation(runTest(snippet, SnippetCompilerArg(SCFlags.Fail, verifyDiagnostics = true)))
  }

  @Test
  def testedFailSnippetsNeedExpectedErrors: Unit = {
    val snippet =
      """|val x = 1 + 1
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Fail, verifyDiagnostics = true))
    assertFailedCompilation(result)
    assertTrue(result.messages.exists(_.message.contains("No errors found when compiling snippet")))
  }

  @Test
  def inlineExpectedDiagnosticMessagesAreIgnored: Unit = {
    val snippet =
      """|val x = 1.missing // error: /totally different/
         |""".stripMargin

    assertSuccessfulCompilation(runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true)))
  }

  @Test
  def inlineExpectationRowMismatchesUseNegTestWording: Unit = {
    val snippet =
      """|val x = 1.missing
         |val y = 1 + 1 // error
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertFailedCompilation(result)
    assertEquals(3, result.messages.count(_.level == MessageLevel.Error))
    assertTrue(result.messages.exists(_.message.contains("Errors found on incorrect row numbers when compiling snippet")))
    assertTrue(result.messages.exists(_.message.contains("Unfulfilled expectation: error on line 2")))
    assertTrue(result.messages.exists(_.message.contains("Unexpected error on line 1")))
  }

  @Test
  def inlineExpectationCountMismatchesUseNegTestWording: Unit = {
    val snippet =
      """|val x = 1.missing // error
         |val y = 1 + 1 // error
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertFailedCompilation(result)
    assertEquals(2, result.messages.count(_.level == MessageLevel.Error))
    assertTrue(result.messages.exists(_.message.contains("Wrong number of errors encountered when compiling snippet")))
    assertTrue(result.messages.exists(_.message.contains("expected: 2, actual: 1")))
    assertTrue(result.messages.exists(_.message.contains("Unfulfilled expectation: error on line 2")))
  }

  @Test
  def inlineExpectationParserAcceptsTrailingProseAndWarnings: Unit = {
    val snippet =
      """|val x = 1.missing // error // explanatory prose
         |val y: Int = try { 5 } // warn // more prose
         |val z = 1.missing // error: not a member
         |""".stripMargin

    val parsed = SnippetExpectations.parse(SnippetSource(snippet, 0), sourceFile(snippet))
    assertTrue(parsed.parserErrors.isEmpty)
    assertEquals(3, parsed.expectations.size)
    assertEquals(2, parsed.expectedErrors)
    assertEquals(1, parsed.expectations.count(_.level == MessageLevel.Warning))
  }

  @Test
  def inlineExpectationParserRejectsAnyposAnnotations: Unit = {
    val snippet =
      """|val x = 1.missing // anypos-error
         |val y: Int = try { 5 } // anypos-warn
         |""".stripMargin

    val parsed = SnippetExpectations.parse(SnippetSource(snippet, 0), sourceFile(snippet))
    val errors = parsed.parserErrors.map(_.message)
    assertEquals(2, parsed.parserErrors.size)
    assertEquals(0, parsed.expectations.size)
    assertTrue(errors.exists(_.contains("Unsupported snippet diagnostic annotation `// anypos-error`; use `// error`")))
    assertTrue(errors.exists(_.contains("Unsupported snippet diagnostic annotation `// anypos-warn`; use `// warn`")))
  }

  @Test
  def inlineExpectedWarningsAreChecked: Unit = {
    val warningSnippet =
      """|val a: Int = try { 5 } // warn
         |""".stripMargin

    val result = runTest(warningSnippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertSuccessfulCompilation(result)
    assertMessageLevelPresent(result, MessageLevel.Warning)
  }

  @Test
  def inlineExpectationWarnRowMismatch: Unit = {
    // Warning occurs on line 1, but annotation is on line 2 — should fail.
    val snippet =
      """|val a: Int = try { 5 }
         |val b = 1 + 1 // warn
         |""".stripMargin

    val result = runTest(snippet, SnippetCompilerArg(SCFlags.Compile, verifyDiagnostics = true))
    assertFailedCompilation(result)
    assertTrue(result.messages.exists(_.message.contains("Warnings found on incorrect row numbers when compiling snippet")))
    assertTrue(result.messages.exists(_.message.contains("Unfulfilled expectation: warning on line 2")))
    assertTrue(result.messages.exists(_.message.contains("Unexpected warning on line 1")))
  }

  @Test
  def multilineInlineExpectationsAreChecked: Unit = {
    val snippet =
      """|import language.experimental.captureChecking
         |import caps.*
         |
         |trait File extends SharedCapability
         |def withFile[T](path: String)(block: File^ => T): T = ???
         |
         |withFile[() => File^]("test.txt"): f =>
         |  () => f  // error // error // error
         |""".stripMargin

    assertSuccessfulCompilation(runTest(snippet, SnippetCompilerArg(SCFlags.Fail, verifyDiagnostics = true)))
  }

}
