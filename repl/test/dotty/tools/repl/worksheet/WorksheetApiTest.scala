package dotty.tools.repl.worksheet

import dotty.tools.repl.worksheet.WorksheetOutput.rendered

import org.junit.Assert.*
import org.junit.Test

import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

class WorksheetApiTest:
  extension (evaluated: interfaces.EvaluatedWorksheet)
    private def messages: List[String] =
      evaluated.diagnostics.asScala.map(_.message).toList

  @Test def reusesASessionIdentifiedByAPath(): Unit =
    val filename = "worksheets/nested/append.worksheet.scala"
    val property = s"scala3.worksheet.append.${java.util.UUID.randomUUID()}"
    val initial =
      s"""val runs = Option(System.getProperty("$property")).fold(1)(_.toInt + 1)
         |System.setProperty("$property", runs.toString)
         |""".stripMargin
    val evaluator = new WorksheetDriver()
    try
      val first = evaluator.evaluate(filename, initial)
      assertEquals(Nil, first.messages)
      assertEquals("1", System.getProperty(property))

      val appended = evaluator.evaluate(filename, initial + "runs + 1\n")
      assertEquals(Nil, appended.messages)
      assertEquals("1", System.getProperty(property))
      assertEquals("res1: Int = 2", appended.statements().get(2).details())
    finally
      evaluator.shutdown()
      System.clearProperty(property)

  @Test def reportsSyntaxErrorsInTheOriginalWorksheetSource(): Unit =
    val filename = "worksheets/syntax.worksheet.scala"
    val text = "val before = 1\nval broken = )\n"
    val evaluator = new WorksheetDriver()
    try
      val result = evaluator.evaluate(filename, text)
      val errors = result.diagnostics().asScala
        .filter(_.level() == dotty.tools.dotc.interfaces.Diagnostic.ERROR)
      assertFalse(result.messages.toString, errors.isEmpty)
      errors.foreach: diagnostic =>
        val position = diagnostic.position().orElseThrow()
        assertEquals(filename, position.source().path())
        assertEquals(text, position.source().textContent())
        assertEquals(1, position.startLine())
    finally evaluator.shutdown()

  @Test def preservesTheDiagnosticPointInAMultilineStatement(): Unit =
    val filename = "diagnostic-point.worksheet.scala"
    val text = "val before = 1\nList(1)\n  .doesNotExist\n"
    val evaluator = new WorksheetDriver()
    try
      val result = evaluator.evaluate(filename, text)
      val errors = result.diagnostics().asScala
        .filter(_.level() == dotty.tools.dotc.interfaces.Diagnostic.ERROR)
      assertEquals(result.messages.toString, 1, errors.size)
      val position = errors.head.position().orElseThrow()
      assertEquals(text.indexOf("doesNotExist"), position.point())
      assertEquals(2, position.line())
      assertEquals(3, position.column())
      assertEquals("  .doesNotExist", position.lineContent().stripLineEnd)
      assertEquals(text.indexOf("List"), position.start())
      assertEquals(text.indexOf("doesNotExist") + "doesNotExist".length, position.end())
      assertEquals(filename, position.source().path())
      assertEquals(text, position.source().textContent())
    finally evaluator.shutdown()

  @Test def cancelsAnEvaluationInProgress(): Unit =
    val property = s"scala3.worksheet.cancel.${java.util.UUID.randomUUID()}"
    val evaluator = new WorksheetDriver()
    val outcome = new AtomicReference[interfaces.EvaluatedWorksheet]()
    val worker = new Thread(() =>
      outcome.set(
        evaluator.evaluate(
          "cancel.worksheet.scala",
          s"""val before = 1
             |var spin = 0L
             |val ticking =
             |  System.setProperty("$property", "running")
             |  while true do spin += 1
             |val after = 2
             |""".stripMargin
        )
      )
    )
    worker.setDaemon(true)
    System.clearProperty(property)
    try
      worker.start()
      val ready = System.currentTimeMillis() + 60000
      while System.getProperty(property) == null && System.currentTimeMillis() < ready do
        Thread.sleep(50)
      assertEquals("the worksheet never started running", "running", System.getProperty(property))

      val deadline = System.currentTimeMillis() + 60000
      while worker.isAlive && System.currentTimeMillis() < deadline do
        evaluator.cancel()
        Thread.sleep(100)

      assertFalse("the evaluation did not stop", worker.isAlive)
      assertEquals(
        """|: Int = 1
           |before: Int = 1
           |
           |: Long = 0L
           |spin: Long = 0L""".stripMargin,
        outcome.get.rendered
      )
      assertEquals(List("The worksheet evaluation was cancelled."), outcome.get.messages)
    finally
      System.clearProperty(property)
      if !worker.isAlive then evaluator.shutdown()

  @Test def cancelsAnEvaluationThatReplacedAnEarlierSession(): Unit =
    val property = s"scala3.worksheet.cancel-reset.${java.util.UUID.randomUUID()}"
    val evaluator = new WorksheetDriver()
    val outcome = new AtomicReference[interfaces.EvaluatedWorksheet]()
    val worker = new Thread(() =>
      outcome.set(
        evaluator.evaluate(
          "second.worksheet.scala",
          s"""val before = 1
             |System.setProperty("$property", "running")
             |var spin = 0L
             |while true do spin += 1
             |""".stripMargin
        )
      )
    )
    worker.setDaemon(true)
    System.clearProperty(property)
    try
      evaluator.evaluate("first.worksheet.scala", "val unrelated = 1\n")

      worker.start()
      val ready = System.currentTimeMillis() + 60000
      while System.getProperty(property) == null && System.currentTimeMillis() < ready do
        Thread.sleep(50)
      assertEquals("the worksheet never started running", "running", System.getProperty(property))

      val deadline = System.currentTimeMillis() + 60000
      while worker.isAlive && System.currentTimeMillis() < deadline do
        evaluator.cancel()
        Thread.sleep(100)

      assertFalse("the evaluation did not stop", worker.isAlive)
      val messages = outcome.get.diagnostics.asScala.map(_.message).toList
      assertEquals(List("The worksheet evaluation was cancelled."), messages)
    finally
      System.clearProperty(property)
      if !worker.isAlive then evaluator.shutdown()

  @Test def acceptsACancellationWhileTheSessionIsStillStarting(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-repl-init-script", "Thread.sleep(3000)"))
    val worker = new Thread(() =>
      evaluator.evaluate("starting.worksheet.scala", "1 + 1\n")
      ()
    )
    worker.setDaemon(true)
    try
      worker.start()
      Thread.sleep(500)

      evaluator.cancel()
      assertFalse(evaluator.isSessionStarted)
    finally
      worker.join(30000)
      evaluator.shutdown()

  @Test def keepsAConfiguredClasspathOption(): Unit =
    val configured = java.nio.file.Path.of(System.getProperty("java.io.tmpdir"))
    val jar = java.nio.file.Path.of(
      classOf[interfaces.EvaluatedWorksheet].getProtectionDomain.getCodeSource.getLocation.toURI
    )
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-classpath", configured.toString))
    try
      val result = evaluator.evaluate(
        "configured-classpath.worksheet.scala",
        s"""//> using jar $jar
           |val held = classOf[dotty.tools.repl.worksheet.interfaces.EvaluatedWorksheet].getSimpleName
           |""".stripMargin
      )

      assertEquals(
        result.diagnostics().asScala.map(_.message()).toString,
        Nil,
        result.diagnostics().asScala.toList
      )
      assertEquals("held: String = \"EvaluatedWorksheet\"", result.statements().get(0).details())
      val reported = result.classpath().asScala.toList
      assertEquals(List(configured, jar), reported)
    finally evaluator.shutdown()

  @Test def startsACompilerSessionOnlyWhenAWorksheetIsEvaluated(): Unit =
    val evaluator = new WorksheetDriver()
    try
      assertFalse(evaluator.isSessionStarted)
      evaluator.shutdown()
      assertFalse(evaluator.isSessionStarted)

      evaluator.evaluate("session.worksheet.scala", "1 + 1\n")
      assertTrue(evaluator.isSessionStarted)
    finally evaluator.shutdown()
    assertFalse(evaluator.isSessionStarted)

  @Test def appliesConfiguredScalacOptions(): Unit =
    val text = "def compute = { val unused = 1; 2 }\n"

    val default = new WorksheetDriver()
    try assertEquals(Nil, default.evaluate("default.worksheet.scala", text).diagnostics().asScala.toList)
    finally default.shutdown()

    val strict = new WorksheetDriver().withScalacOptions(List("-Wunused:all").asJava)
    try
      val diagnostics = strict.evaluate("strict.worksheet.scala", text).diagnostics().asScala.toList
      assertEquals(List("unused local definition"), diagnostics.map(_.message))
    finally strict.shutdown()

  @Test def formatsSummariesForTheConfiguredScreenWidth(): Unit =
    val text = "val letters = List.fill(40)(\"abc\").mkString\n"

    val wide = new WorksheetDriver().withScreenWidth(200)
    val narrow = new WorksheetDriver().withScreenWidth(30)
    try
      val wideStatement = wide.evaluate("wide.worksheet.scala", text).statements().get(0)
      val narrowStatement = narrow.evaluate("narrow.worksheet.scala", text).statements().get(0)

      assertTrue(wideStatement.summary(), wideStatement.isSummaryComplete)
      assertFalse(narrowStatement.summary(), narrowStatement.isSummaryComplete)
      assertTrue(narrowStatement.summary().length < wideStatement.summary().length)
      assertEquals(wideStatement.details(), narrowStatement.details())
    finally
      wide.shutdown()
      narrow.shutdown()

  @Test def reportsCompilerOptionsThatAreNotRecognised(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-Wnosuchthing"))
    try
      val result = evaluator.evaluate("unknown.worksheet.scala", "val x = 40\n")
      assertEquals(1, result.statements().size)
      val diagnostic = result.diagnostics().get(0)
      assertEquals(dotty.tools.dotc.interfaces.Diagnostic.WARNING, diagnostic.level())
      assertEquals(List("bad option '-Wnosuchthing' was ignored"), result.messages)
    finally evaluator.shutdown()

  @Test def reportsNothingExtraForValidCompilerOptions(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-Wunused:all"))
    try
      val result = evaluator.evaluate("valid-options.worksheet.scala", "val x = 40\n")
      assertEquals(List(), result.diagnostics().asScala.map(_.message()).toList)
    finally evaluator.shutdown()

  @Test def reportsCompilerOptionsWithAnInvalidValue(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-source:definitely-not-a-source-version"))
    try
      val result = evaluator.evaluate("bad-value.worksheet.scala", "val x = 40\n")
      assertEquals(0, result.statements().size)
      assertEquals(1, result.diagnostics().size)
      val diagnostic = result.diagnostics().get(0)
      assertEquals(dotty.tools.dotc.interfaces.Diagnostic.ERROR, diagnostic.level())
      assertEquals(
        """definitely-not-a-source-version is not a valid choice for -source.
          |Expected a source version.
          |Available choices: 3.0-migration, 3.0, 3.1, 3.2-migration, 3.2, 3.3-migration, 3.3, 3.4-migration, 3.4, 3.5-migration, 3.5, 3.6-migration, 3.6, 3.7-migration, 3.7, 3.8-migration, 3.8, 3.9-migration, 3.9, 3.10-migration, 3.10, 3.11-migration, 3.11, future-migration, future
          |scala -help  gives more information""".stripMargin,
        diagnostic.message()
      )
    finally evaluator.shutdown()

  @Test def keepsConfigurationDiagnosticsWhenTheTextDoesNotParse(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-Ybest-effort"))
    try
      val messages = evaluator
        .evaluate("unparseable.worksheet.scala", "val x = (\n")
        .diagnostics().asScala.map(_.message()).toList
      assertEquals(
        List(
          "Options incompatible with repl will be ignored: -Ybest-effort",
          s"expression expected but ${Console.RED}eof${Console.RESET} found"
        ),
        messages
      )
    finally evaluator.shutdown()
