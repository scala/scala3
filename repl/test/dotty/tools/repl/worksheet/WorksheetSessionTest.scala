package dotty.tools.repl.worksheet

import dotty.tools.repl.ReplTest

import org.junit.Assert.*
import org.junit.After
import org.junit.Test

import scala.jdk.CollectionConverters.*

class WorksheetSessionTest:
  private val driver = new WorksheetSession(ReplTest.defaultOptions)

  @After def shutdownDriver(): Unit = driver.shutdown()

  @Test def evaluatesDefinitionsAndExpressionsInOneProgram(): Unit =
    val result = driver.evaluate(
      "values.worksheet.scala",
      """val x = 40
        |val y = x + 2
        |y * 2
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(3, result.statements.length)
    assertEquals(": Int = 40", result.statements(0).summary)
    assertEquals("x: Int = 40", result.statements(0).details)
    assertEquals(": Int = 42", result.statements(1).summary)
    assertEquals("y: Int = 42", result.statements(1).details)
    assertEquals(": Int = 84", result.statements(2).summary)
    assertEquals("res0: Int = 84", result.statements(2).details)

  @Test def assignsOutputToTheStatementThatProducedIt(): Unit =
    val result = driver.evaluate(
      "output.worksheet.scala",
      """println("hello")
        |val answer = 42
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(2, result.statements.length)
    assertEquals("hello", result.statements.head.summary)
    assertEquals("// hello", result.statements.head.details)
    assertEquals(": Int = 42", result.statements(1).summary)

  @Test def supportsImportsMultilineExpressionsAndPatternDefinitions(): Unit =
    val result = driver.evaluate(
      "syntax.worksheet.scala",
      """import scala.concurrent.duration.*
        |val (number, text) = (1, "two")
        |List(
        |  number,
        |  text.length
        |).sum
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(2, result.statements.length)
    assertEquals("number: Int = 1, text: String = \"two\"", result.statements.head.summary)
    assertEquals("number: Int = 1\ntext: String = \"two\"", result.statements.head.details)
    assertEquals(": Int = 4", result.statements(1).summary)

  @Test def stopsAtACompilationError(): Unit =
    val result = driver.evaluate(
      "error.worksheet.scala",
      """println("runs")
        |val broken: String = 1
        |val unreachable = 2
        |""".stripMargin
    )

    assertEquals(
      List(1),
      result.diagnostics
        .filter(_.severity == WorksheetDiagnosticSeverity.Error)
        .map(_.position.startLine)
    )
    assertEquals(List("// runs"), result.statements.map(_.details))

  @Test def stopsAtAnException(): Unit =
    val result = driver.evaluate(
      "exception.worksheet.scala",
      """val before = 1
        |throw new RuntimeException("boom")
        |val after = 2
        |""".stripMargin
    )

    assertTrue(
      result.diagnostics.toString,
      result.diagnostics.exists(_.message.contains("RuntimeException: boom"))
    )
    assertEquals(List("before: Int = 1"), result.statements.map(_.details))

  @Test def reportsDirectivesAsIgnored(): Unit =
    val result = driver.evaluate(
      "directive.worksheet.scala",
      """//> using dep com.lihaoyi::os-lib:0.11.8
        |1 + 1
        |""".stripMargin
    )

    assertEquals(1, result.statements.length)
    val warning = result.diagnostics.find(_.message == "REPL Worksheet PoC")
    assertTrue(result.diagnostics.toString, warning.isDefined)
    assertEquals(WorksheetDiagnosticSeverity.Warning, warning.get.severity)
    assertEquals(0, warning.get.position.startLine)
    assertEquals(0, warning.get.position.startColumn)
    assertEquals(0, warning.get.position.endLine)
    assertEquals(
      "//> using dep com.lihaoyi::os-lib:0.11.8".length,
      warning.get.position.endColumn
    )

  @Test def allowsShadowingBetweenReplSubmissions(): Unit =
    val result = driver.evaluate(
      "shadowing.worksheet.scala",
      """val value = 1
        |val value = 2
        |value
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(3, result.statements.length)
    assertEquals("value: Int = 1", result.statements(0).details)
    assertEquals("value: Int = 2", result.statements(1).details)
    assertEquals("res0: Int = 2", result.statements(2).details)

  @Test def evaluatesOnlyTheAppendedStatements(): Unit =
    val property = s"scala3.worksheet.append.${java.util.UUID.randomUUID()}"
    val initial =
      s"""val runCount = Option(System.getProperty("$property")).fold(1)(_.toInt + 1)
         |System.setProperty("$property", runCount.toString)
         |val value = 1
         |""".stripMargin

    System.clearProperty(property)
    try
      val first = driver.evaluate("append.worksheet.scala", initial)
      assertEquals(Nil, first.diagnostics)
      assertEquals("1", System.getProperty(property))

      val second = driver.evaluate(
        "append.worksheet.scala",
        initial +
          """val value = 2
            |value
            |""".stripMargin
      )

      assertEquals(Nil, second.diagnostics)
      assertEquals("1", System.getProperty(property))
      assertEquals("value: Int = 2", second.statements.takeRight(2).head.details)
      assertEquals("res1: Int = 2", second.statements.last.details)
    finally System.clearProperty(property)

  @Test def appendsFromTheStatementThatFailedToCompile(): Unit =
    val property = s"scala3.worksheet.compile-error.${java.util.UUID.randomUUID()}"
    val initial = "val before = 1\n"

    System.clearProperty(property)
    try
      val first = driver.evaluate("append-error.worksheet.scala", initial)
      assertEquals(Nil, first.diagnostics)

      val broken =
        initial +
          s"""System.setProperty("$property", "executed")
             |val broken: String = 1
             |""".stripMargin
      val second = driver.evaluate("append-error.worksheet.scala", broken)
      assertTrue(second.diagnostics.exists(_.severity == WorksheetDiagnosticSeverity.Error))
      assertEquals("executed", System.getProperty(property))

      System.setProperty(property, "not repeated")
      val third = driver.evaluate(
        "append-error.worksheet.scala",
        broken.replace("val broken: String = 1", """val fixed: String = "ok"""")
      )
      assertEquals(Nil, third.diagnostics)
      assertEquals("not repeated", System.getProperty(property))
      assertEquals("fixed: String = \"ok\"", third.statements.last.details)
    finally System.clearProperty(property)

  @Test def bindsExpressionResultsToReusableResValues(): Unit =
    val result = driver.evaluate(
      "res.worksheet.scala",
      """1 + 1
        |res0 + 1
        |val doubled = res1 * 2
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(3, result.statements.length)
    assertEquals("res0: Int = 2", result.statements(0).details)
    assertEquals("res1: Int = 3", result.statements(1).details)
    assertEquals("doubled: Int = 6", result.statements(2).details)

  @Test def keepsResNumbersTakenByAFlattenedBlock(): Unit =
    val result = driver.evaluate(
      "block-res.worksheet.scala",
      """{ println("a"); 42 }
        |1 + 1
        |res1
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals("res1: Int = 42", result.statements(0).details.linesIterator.next())
    assertEquals("res2: Int = 2", result.statements(1).details)
    assertEquals("res3: Int = 42", result.statements(2).details)

  @Test def reusesResNumbersLeftByUnitExpressions(): Unit =
    val result = driver.evaluate(
      "unit-res.worksheet.scala",
      """1 + 1
        |println("hi")
        |res0 + 1
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals("res0: Int = 2", result.statements(0).details)
    assertEquals("// hi", result.statements(1).details)
    assertEquals("res1: Int = 3", result.statements(2).details)

  @Test def propagatesGlobalLanguageImportsToLaterStatements(): Unit =
    val result = driver.evaluate(
      "language.worksheet.scala",
      """import language.experimental.pureFunctions
        |val increment: Int -> Int = x => x + 1
        |increment(41)
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(": Int = 42", result.statements.last.summary)

  @Test def retainsTheDiagnosticsOfAnUnchangedPrefix(): Unit =
    val initial =
      """def describe(flag: Boolean) = flag match
        |  case true => "yes"
        |""".stripMargin

    val first = driver.evaluate("warning.worksheet.scala", initial)
    assertTrue(
      first.diagnostics.toString,
      first.diagnostics.exists(_.severity == WorksheetDiagnosticSeverity.Warning)
    )

    val second = driver.evaluate("warning.worksheet.scala", initial + "val value = 1\n")

    assertEquals(first.diagnostics, second.diagnostics)
    assertEquals("value: Int = 1", second.statements.last.details)

  @Test def resetsTheSessionWhenAnEarlierStatementChanges(): Unit =
    val initial =
      """def describe(flag: Boolean) = flag match
        |  case true => "yes"
        |1 + 1
        |""".stripMargin

    val first = driver.evaluate("reset.worksheet.scala", initial)
    assertTrue(first.diagnostics.nonEmpty)
    assertEquals("res0: Int = 2", first.statements.last.details)

    val second = driver.evaluate("reset.worksheet.scala", "2 + 2\n")

    assertEquals(Nil, second.diagnostics)
    assertEquals(1, second.statements.length)
    assertEquals("res0: Int = 4", second.statements.head.details)

  @Test def reEvaluatingIdenticalTextReplaysTheCachedResult(): Unit =
    val property = s"scala3.worksheet.identical.${java.util.UUID.randomUUID()}"
    val text =
      s"""System.setProperty("$property", "executed")
         |val value = 1
         |""".stripMargin

    System.clearProperty(property)
    try
      val first = driver.evaluate("identical.worksheet.scala", text)
      assertEquals(Nil, first.diagnostics)
      assertEquals("executed", System.getProperty(property))
      System.clearProperty(property)

      val second = driver.evaluate("identical.worksheet.scala", text)

      assertEquals(first.diagnostics, second.diagnostics)
      assertEquals(first.statements, second.statements)
      assertEquals(null, System.getProperty(property))
    finally System.clearProperty(property)

  @Test def keepsReportingARuntimeFailureWhileTheWorksheetGrows(): Unit =
    val initial =
      """val before = 1
        |throw new RuntimeException("boom")
        |""".stripMargin

    val first = driver.evaluate("growing.worksheet.scala", initial)
    assertTrue(first.diagnostics.toString, first.diagnostics.exists(_.message.contains("boom")))

    val second = driver.evaluate("growing.worksheet.scala", initial + "val after = 2\n")
    assertTrue(second.diagnostics.toString, second.diagnostics.exists(_.message.contains("boom")))
    assertEquals(List("before: Int = 1"), second.statements.map(_.details))

  @Test def reportsReassignmentsUnderTheAssignedName(): Unit =
    val result = driver.evaluate(
      "assign.worksheet.scala",
      """var counter = 1
        |counter = counter + 1
        |counter
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(3, result.statements.length)
    assertEquals("counter: Int = 1", result.statements(0).details)
    assertEquals("counter: Int = 2", result.statements(1).details)
    assertEquals("res0: Int = 2", result.statements(2).details)

  @Test def marksASummaryIncompleteWhenTheStatementAlsoPrinted(): Unit =
    val result = driver.evaluate(
      "output-and-value.worksheet.scala",
      """val value =
        |  println("noticed")
        |  1
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertEquals(1, result.statements.length)
    assertEquals(": Int = 1", result.statements.head.summary)
    assertEquals("value: Int = 1\n// noticed", result.statements.head.details)
    assertFalse(result.statements.head.isSummaryComplete)

  @Test def doesNotReportTheCompilerSummaryAsADiagnostic(): Unit =
    val result = driver.evaluate(
      "summary.worksheet.scala",
      """def describe(flag: Boolean) = flag match
        |  case true => "yes"
        |""".stripMargin
    )

    assertEquals(result.diagnostics.map(_.message).toString, 1, result.diagnostics.length)
    assertEquals(WorksheetDiagnosticSeverity.Warning, result.diagnostics.head.severity)
    assertTrue(result.diagnostics.head.position.startLine >= 0)

  @Test def resetsTheSessionWhenAnotherWorksheetIsEvaluated(): Unit =
    val first = driver.evaluate(
      "first.worksheet.scala",
      """1 + 1
        |val broken: String = 2
        |""".stripMargin
    )
    assertTrue(first.diagnostics.exists(_.severity == WorksheetDiagnosticSeverity.Error))

    val second = driver.evaluate("second.worksheet.scala", "1 + 1\n")

    assertEquals(Nil, second.diagnostics)
    assertEquals(1, second.statements.length)
    assertEquals("res0: Int = 2", second.statements.head.details)

  @Test def anchorsDirectiveWarningsOnTheDirectiveThatCausedThem(): Unit =
    val result = driver.evaluate(
      "directive-position.worksheet.scala",
      """val leading = 1
        |//> using dep
        |1 + 1
        |""".stripMargin
    )

    assertTrue(
      result.diagnostics.toString,
      result.diagnostics.forall(_.position.startLine == 1)
    )
    assertTrue(result.diagnostics.nonEmpty)

  @Test def reportsUnsupportedDirectiveKeysAsIgnoredToo(): Unit =
    val result = driver.evaluate(
      "unsupported-directive.worksheet.scala",
      """//> using scala "3.7.0"
        |1 + 1
        |""".stripMargin
    )

    assertEquals(1, result.statements.length)
    assertEquals(List("REPL Worksheet PoC"), result.diagnostics.map(_.message))
    assertEquals(0, result.diagnostics.head.position.startLine)

  @Test def treatsAnEmptyFilenameAsAnOrdinaryWorksheet(): Unit =
    val first = driver.evaluate("", "1 + 1\n")
    assertEquals("res0: Int = 2", first.statements.head.details)

    val second = driver.evaluate("", "2 + 2\n")

    assertEquals(Nil, second.diagnostics)
    assertEquals(1, second.statements.length)
    assertEquals("res0: Int = 4", second.statements.head.details)

  @Test def keepsValueNumberingAcrossAFailedAppend(): Unit =
    val initial = "val x = 1\n"
    assertEquals(Nil, driver.evaluate("numbering.worksheet.scala", initial).diagnostics)

    val broken = driver.evaluate(
      "numbering.worksheet.scala",
      initial + "1 + 1\nval bad: String = 1\n"
    )
    assertTrue(broken.diagnostics.exists(_.severity == WorksheetDiagnosticSeverity.Error))

    val fixed = driver.evaluate(
      "numbering.worksheet.scala",
      initial + "1 + 1\nval good = 2\n"
    )
    assertEquals(Nil, fixed.diagnostics)
    assertEquals("res0: Int = 2", fixed.statements(1).details)

  @Test def reportsReplCommandsAsUnsupported(): Unit =
    val result = driver.evaluate("command.worksheet.scala", ":quit\n")

    assertTrue(result.diagnostics.toString, result.diagnostics.nonEmpty)
    assertEquals(Nil, result.statements)

  @Test def rebuildsASessionLeftByAnotherWorksheetsSyntaxError(): Unit =
    val property = s"scala3.worksheet.stale.${java.util.UUID.randomUUID()}"
    val first = s"""System.setProperty("$property", "1")
                   |val value = 1
                   |""".stripMargin

    System.clearProperty(property)
    try
      assertEquals(Nil, driver.evaluate("a.worksheet.scala", first).diagnostics)
      assertEquals("1", System.getProperty(property))

      val broken = driver.evaluate("b.worksheet.scala", "val oops = (\n")
      assertTrue(broken.diagnostics.nonEmpty)
      assertEquals(Nil, broken.statements)

      System.clearProperty(property)
      val again = driver.evaluate("a.worksheet.scala", first)

      assertEquals(Nil, again.diagnostics)
      assertEquals("1", System.getProperty(property))
    finally System.clearProperty(property)

  @Test def leavesTheProcessWideStreamsAlone(): Unit =
    val originalOut = System.out
    val originalErr = System.err
    val result = driver.evaluate("streams.worksheet.scala", "println(\"hello\")\n")

    assertEquals("hello", result.statements.head.summary)
    assertSame(originalOut, System.out)
    assertSame(originalErr, System.err)

  @Test def capturesOutputFromThreadsTheWorksheetStarts(): Unit =
    val result = driver.evaluate(
      "threads.worksheet.scala",
      """val answer =
        |  val worker = new Thread(() => println("child-output"))
        |  worker.start()
        |  worker.join()
        |  42
        |""".stripMargin
    )

    assertEquals(Nil, result.diagnostics)
    assertTrue(result.statements.head.details, result.statements.head.details.contains("child-output"))

  @Test def rebuildsASessionAfterACommandReplacesTheSameWorksheet(): Unit =
    val property = s"scala3.worksheet.same-file.${java.util.UUID.randomUUID()}"
    val text = s"""System.setProperty("$property", "1")
                  |val value = 1
                  |""".stripMargin

    System.clearProperty(property)
    try
      assertEquals(Nil, driver.evaluate("same.worksheet.scala", text).diagnostics)
      assertEquals("1", System.getProperty(property))

      assertTrue(driver.evaluate("same.worksheet.scala", ":quit\n").diagnostics.nonEmpty)

      System.clearProperty(property)
      val again = driver.evaluate("same.worksheet.scala", text)

      assertEquals(Nil, again.diagnostics)
      assertEquals("1", System.getProperty(property))
    finally System.clearProperty(property)
