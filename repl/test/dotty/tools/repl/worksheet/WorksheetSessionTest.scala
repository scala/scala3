package dotty.tools.repl.worksheet

import dotty.tools.repl.ReplTest

import WorksheetDiagnosticSeverity.Warning

import org.junit.Assert.*
import org.junit.After
import org.junit.Test

import scala.jdk.CollectionConverters.*

class WorksheetSessionTest:
  private val driver = new WorksheetSession(ReplTest.defaultOptions)

  @After def shutdownDriver(): Unit = driver.shutdown()

  private def withProperty[A](body: String => A): A =
    val property = s"scala3.worksheet.${java.util.UUID.randomUUID()}"
    System.clearProperty(property)
    try body(property) finally System.clearProperty(property)

  extension (result: WorksheetResult)
    private def errors: List[WorksheetDiagnostic] =
      result.diagnostics.filter(_.severity == WorksheetDiagnosticSeverity.Error)

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

  @Test def reportsLazyValuesAndGivensWithoutEvaluatingThem(): Unit = withProperty: lazyProperty =>
    withProperty: givenProperty =>
      val result = driver.evaluate(
        "lazy.worksheet.scala",
        s"""lazy val value: Unit = System.setProperty("$lazyProperty", "evaluated")
           |given ordering: Ordering[Int] =
           |  System.setProperty("$givenProperty", "evaluated")
           |  Ordering.Int
           |""".stripMargin
      )

      assertEquals(Nil, result.diagnostics)
      assertEquals(
        List("lazy val value: Unit", "lazy val ordering: Ordering[Int]"),
        result.statements.map(_.details)
      )
      assertEquals(result.statements.map(_.details), result.statements.map(_.summary))
      assertTrue(result.statements.forall(_.isSummaryComplete))
      assertNull(System.getProperty(lazyProperty))
      assertNull(System.getProperty(givenProperty))

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

    assertEquals(List(1), result.errors.map(_.position.startLine))
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

  @Test def honoursTheDependencyDirective(): Unit =
    val result = driver.evaluate(
      "dependency.worksheet.scala",
      """//> using dep com.lihaoyi::os-lib:0.11.8
        |import os.*
        |val separator = os.pwd.toString.head
        |""".stripMargin
    )

    assertEquals(result.diagnostics.toString, Nil, result.diagnostics)
    assertTrue(
      result.statements.last.details,
      result.statements.last.details.startsWith("separator: Char = ")
    )
    assertEquals(
      List(WorksheetDependency("com.lihaoyi", "os-lib_3", "0.11.8")),
      result.dependencies
    )
    assertTrue(result.classpath.toString, result.classpath.nonEmpty)

  @Test def reportsADependencyThatCannotBeResolved(): Unit =
    val result = driver.evaluate(
      "unresolvable.worksheet.scala",
      """//> using dep com.lihaoyi::os-lib:0.0.0-does-not-exist
        |1 + 1
        |""".stripMargin
    )

    val errors = result.errors
    assertEquals(result.diagnostics.toString, 1, errors.length)
    assertEquals(0, errors.head.position.startLine)
    assertTrue(errors.head.message, errors.head.message.contains("Unable to resolve"))
    assertEquals(Nil, result.dependencies)
    assertEquals(List("res0: Int = 2"), result.statements.map(_.details))

  @Test def honoursTheJarDirective(): Unit =
    val jar = WorksheetSessionTest.interfacesJar
    val result = driver.evaluate(
      "jar.worksheet.scala",
      s"""//> using jar $jar
         |val held = classOf[dotty.tools.repl.worksheet.interfaces.RangePosition].getSimpleName
         |""".stripMargin
    )

    assertEquals(result.diagnostics.toString, Nil, result.diagnostics)
    assertEquals("held: String = \"RangePosition\"", result.statements.last.details)
    assertTrue(result.classpath.toString, result.classpath.contains(jar))

  @Test def reportsAJarThatDoesNotExist(): Unit =
    val result = driver.evaluate(
      "missing-jar.worksheet.scala",
      """//> using jar /does/not/exist.jar
        |1 + 1
        |""".stripMargin
    )

    val errors = result.errors
    assertEquals(result.diagnostics.toString, 1, errors.length)
    assertTrue(errors.head.message, errors.head.message.contains("does not exist"))
    assertEquals(List("res0: Int = 2"), result.statements.map(_.details))

  @Test def reportsADirectiveThatWorksheetsDoNotSupport(): Unit =
    val result = driver.evaluate(
      "unsupported.worksheet.scala",
      """//> using scala 3.7.0
        |1 + 1
        |""".stripMargin
    )

    assertEquals(1, result.statements.length)
    assertEquals(
      List("The `using scala` directive is not supported in worksheets."),
      result.diagnostics.map(_.message)
    )
    assertEquals(0, result.diagnostics.head.position.startLine)

  @Test def resolvesDirectivesOnlyOncePerSession(): Unit =
    val initial =
      """//> using dep com.lihaoyi::os-lib:0.11.8
        |val first = os.pwd.toString.nonEmpty
        |""".stripMargin

    val first = driver.evaluate("once.worksheet.scala", initial)
    assertEquals(first.diagnostics.toString, Nil, first.diagnostics)

    val second = driver.evaluate("once.worksheet.scala", initial + "val second = os.pwd.toString.nonEmpty\n")

    assertEquals(first.dependencies, second.dependencies)
    assertEquals(first.classpath, second.classpath)
    assertEquals("second: Boolean = true", second.statements.last.details)

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

  @Test def evaluatesOnlyTheAppendedStatements(): Unit = withProperty: property =>
    val initial =
      s"""val runCount = Option(System.getProperty("$property")).fold(1)(_.toInt + 1)
         |System.setProperty("$property", runCount.toString)
         |val value = 1
         |""".stripMargin

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

  @Test def appendsFromTheStatementThatFailedToCompile(): Unit = withProperty: property =>
    val initial = "val before = 1\n"

    val first = driver.evaluate("append-error.worksheet.scala", initial)
    assertEquals(Nil, first.diagnostics)

    val broken =
      initial +
        s"""System.setProperty("$property", "executed")
           |val broken: String = 1
           |""".stripMargin
    val second = driver.evaluate("append-error.worksheet.scala", broken)
    assertTrue(second.errors.nonEmpty)
    assertEquals("executed", System.getProperty(property))

    System.setProperty(property, "not repeated")
    val third = driver.evaluate(
      "append-error.worksheet.scala",
      broken.replace("val broken: String = 1", """val fixed: String = "ok"""")
    )
    assertEquals(Nil, third.diagnostics)
    assertEquals("not repeated", System.getProperty(property))
    assertEquals("fixed: String = \"ok\"", third.statements.last.details)

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
    assertTrue(first.diagnostics.toString, first.diagnostics.exists(_.severity == Warning))

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

  @Test def reEvaluatingIdenticalTextReplaysTheCachedResult(): Unit = withProperty: property =>
    val text =
      s"""System.setProperty("$property", "executed")
         |val value = 1
         |""".stripMargin

    val first = driver.evaluate("identical.worksheet.scala", text)
    assertEquals(Nil, first.diagnostics)
    assertEquals("executed", System.getProperty(property))
    System.clearProperty(property)

    val second = driver.evaluate("identical.worksheet.scala", text)

    assertEquals(first.diagnostics, second.diagnostics)
    assertEquals(first.statements, second.statements)
    assertEquals(null, System.getProperty(property))

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
    assertTrue(first.errors.nonEmpty)

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

    assertTrue(result.diagnostics.toString, result.diagnostics.nonEmpty)
    assertTrue(
      result.diagnostics.toString,
      result.diagnostics.forall(_.position.startLine == 1)
    )

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
    assertTrue(broken.errors.nonEmpty)

    val fixed = driver.evaluate(
      "numbering.worksheet.scala",
      initial + "1 + 1\nval good = 2\n"
    )
    assertEquals(Nil, fixed.diagnostics)
    assertEquals("res0: Int = 2", fixed.statements(1).details)

  @Test def resolvesDirectivesAppendedToAnExistingSession(): Unit =
    val filename = "appended-directives.worksheet.scala"
    val declared = "//> using dep com.lihaoyi::fansi:0.5.0\n"

    assertEquals(Nil, driver.evaluate(filename, declared).diagnostics)

    val result = driver.evaluate(
      filename,
      s"""$declared//> using dep com.lihaoyi::os-lib:0.11.8
         |val here = os.pwd.toString.nonEmpty
         |""".stripMargin
    )

    assertEquals(result.diagnostics.toString, Nil, result.diagnostics)
    assertEquals("here: Boolean = true", result.statements.last.details)
    assertEquals(
      List("com.lihaoyi" -> "fansi_3", "com.lihaoyi" -> "os-lib_3"),
      result.dependencies.map(dependency => dependency.organization -> dependency.moduleName)
    )

  @Test def runsCompleteStatementsBeforeASyntaxError(): Unit =
    val filename = "prefix.worksheet.scala"
    assertEquals(
      List("before: Int = 1"),
      driver.evaluate(filename, "val before = 1\n").statements.map(_.details)
    )

    val result = driver.evaluate(
      filename,
      """val before = 1
        |val after = 2
        |val oops = (
        |""".stripMargin
    )

    assertTrue(result.diagnostics.toString, result.diagnostics.nonEmpty)
    assertEquals(
      List("before: Int = 1", "after: Int = 2"),
      result.statements.map(_.details)
    )

  @Test def anchorsEachDirectiveDiagnosticToItsOwnLine(): Unit =
    val result = driver.evaluate(
      "directive-lines.worksheet.scala",
      """//> using jar /does/not/exist.jar
        |//> using scala 3.7.0
        |//> using dep not-a-coordinate
        |1 + 1
        |""".stripMargin
    )

    assertEquals(
      result.diagnostics.map(_.message).toString,
      List(0, 1, 2),
      result.diagnostics.map(_.position.startLine).sorted
    )

  @Test def marksASummaryThatDropsFormattingAsIncomplete(): Unit =
    val result = driver.evaluate("spaced.worksheet.scala", "val spaced = \"a  b\"\n")

    assertEquals("spaced: String = \"a  b\"", result.statements.head.details)
    assertEquals(": String = \"a b\"", result.statements.head.summary)
    assertFalse(result.statements.head.summary, result.statements.head.isSummaryComplete)

  @Test def ignoresACancellationRequestedWhileNothingRuns(): Unit =
    driver.cancel()

    val result = driver.evaluate("idle-cancel.worksheet.scala", "1 + 1\n")

    assertEquals(result.diagnostics.toString, Nil, result.diagnostics)
    assertEquals(List("res0: Int = 2"), result.statements.map(_.details))

  @Test def runsStatementsThatBindNothing(): Unit = withProperty: wildcard =>
    withProperty: ascribed =>
      val result = driver.evaluate(
        "no-binder.worksheet.scala",
        s"""val _ = { System.setProperty("$wildcard", "ran"); 1 }
           |val _: Int = { System.setProperty("$ascribed", "ran"); 2 }
           |""".stripMargin
      )

      assertEquals(result.diagnostics.toString, Nil, result.diagnostics)
      assertEquals("ran", System.getProperty(wildcard))
      assertEquals("ran", System.getProperty(ascribed))

  @Test def runsCompleteStatementsOfANewWorksheetBeforeASyntaxError(): Unit =
    assertEquals(Nil, driver.evaluate("earlier.worksheet.scala", "val unrelated = 1\n").diagnostics)

    val result = driver.evaluate(
      "later.worksheet.scala",
      """val before = 2
        |val bad = (
        |""".stripMargin
    )

    assertTrue(result.diagnostics.toString, result.diagnostics.nonEmpty)
    assertEquals(List("before: Int = 2"), result.statements.map(_.details))

  @Test def reportsTheClasspathOfANewWorksheetThatDoesNotParse(): Unit =
    val jar = WorksheetSessionTest.interfacesJar
    assertEquals(Nil, driver.evaluate("other.worksheet.scala", "val unrelated = 1\n").diagnostics)

    val result = driver.evaluate(
      "broken-jar.worksheet.scala",
      s"""//> using jar $jar
         |val bad = (
         |""".stripMargin
    )

    assertTrue(result.diagnostics.toString, result.diagnostics.nonEmpty)
    assertTrue(result.classpath.toString, result.classpath.contains(jar))

  @Test def reportsNoClasspathForAnInputThatIsNotAWorksheet(): Unit =
    val filename = "then-command.worksheet.scala"
    val jar = WorksheetSessionTest.interfacesJar
    val evaluated = driver.evaluate(filename, s"//> using jar $jar\nval value = 1\n")
    assertTrue(evaluated.classpath.toString, evaluated.classpath.contains(jar))

    val result = driver.evaluate(filename, ":quit\n")

    assertTrue(result.diagnostics.toString, result.diagnostics.nonEmpty)
    assertEquals(Nil, result.statements)
    assertEquals(Nil, result.classpath)
    assertEquals(Nil, result.dependencies)

  @Test def reportsADirectiveAppendedAfterAnUnsupportedOne(): Unit =
    val filename = "appended-unsupported.worksheet.scala"
    val first = "//> using scala 3.7.0\n"
    assertEquals(1, driver.evaluate(filename, first).diagnostics.length)

    val result = driver.evaluate(filename, s"${first}//> using platform jvm\n1 + 1\n")

    assertEquals(
      result.diagnostics.map(_.message).toString,
      List(0, 1),
      result.diagnostics.map(_.position.startLine).sorted
    )

  @Test def keepsADiagnosticForEachLineThatCausedIt(): Unit =
    val result = driver.evaluate(
      "repeated.worksheet.scala",
      """1 + 1
        |//> using platform jvm
        |//> using platform jvm
        |""".stripMargin
    )

    assertEquals(
      result.diagnostics.map(_.message).toString,
      List(1, 2),
      result.diagnostics.map(_.position.startLine).sorted
    )

  @Test def doesNotKeepAWarningFromAStatementThatFailed(): Unit =
    val strict = new WorksheetSession(ReplTest.defaultOptions ++ Array("-Wunused:all"))
    val text = """val value = { val unused = 1; throw new RuntimeException("boom") }
                 |""".stripMargin
    def unusedWarnings(result: WorksheetResult): List[String] =
      result.diagnostics.filter(_.message.contains("unused")).map(_.message)
    try
      assertEquals(1, unusedWarnings(strict.evaluate("warned.worksheet.scala", text)).length)

      val again = strict.evaluate("warned.worksheet.scala", text)

      assertEquals(unusedWarnings(again).toString, 1, unusedWarnings(again).length)
    finally strict.shutdown()

  @Test def rebuildsASessionLeftByAnotherWorksheetsSyntaxError(): Unit = withProperty: property =>
    val first = s"""System.setProperty("$property", "1")
                   |val value = 1
                   |""".stripMargin

    assertEquals(Nil, driver.evaluate("a.worksheet.scala", first).diagnostics)
    assertEquals("1", System.getProperty(property))

    val broken = driver.evaluate("b.worksheet.scala", "val oops = (\n")
    assertTrue(broken.diagnostics.nonEmpty)
    assertEquals(Nil, broken.statements)

    System.clearProperty(property)
    val again = driver.evaluate("a.worksheet.scala", first)

    assertEquals(Nil, again.diagnostics)
    assertEquals("1", System.getProperty(property))

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

  @Test def rebuildsASessionAfterACommandReplacesTheSameWorksheet(): Unit = withProperty: property =>
    val text = s"""System.setProperty("$property", "1")
                  |val value = 1
                  |""".stripMargin

    assertEquals(Nil, driver.evaluate("same.worksheet.scala", text).diagnostics)
    assertEquals("1", System.getProperty(property))

    assertTrue(driver.evaluate("same.worksheet.scala", ":quit\n").diagnostics.nonEmpty)

    System.clearProperty(property)
    val again = driver.evaluate("same.worksheet.scala", text)

    assertEquals(Nil, again.diagnostics)
    assertEquals("1", System.getProperty(property))

private object WorksheetSessionTest:
  val interfacesJar: java.nio.file.Path =
    java.nio.file.Path.of(
      classOf[interfaces.RangePosition].getProtectionDomain.getCodeSource.getLocation.toURI
    )
