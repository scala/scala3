package dotty.tools.repl.worksheet

import org.junit.Assert.*
import org.junit.Test

import java.util.concurrent.atomic.AtomicReference

import scala.jdk.CollectionConverters.*

class WorksheetApiTest:
  @Test def cancelsAnEvaluationInProgress(): Unit =
    val property = s"scala3.worksheet.cancel.${java.util.UUID.randomUUID()}"
    val evaluator = new WorksheetDriver()
    val outcome = new AtomicReference[interfaces.EvaluatedWorksheet]()
    val worker = new Thread(() =>
      outcome.set(
        evaluator.evaluate(
          "cancel.worksheet.scala",
          s"""val before = 1
             |System.setProperty("$property", "running")
             |var spin = 0L
             |while true do spin += 1
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
      val details = outcome.get.statements.asScala.map(_.details).toList
      assertFalse(details.mkString("\n"), details.exists(_.startsWith("after:")))
      assertTrue(details.mkString("\n"), details.headOption.contains("before: Int = 1"))
      val messages = outcome.get.diagnostics.asScala.map(_.message).toList
      assertTrue(messages.mkString("\n"), messages.exists(_.contains("cancelled")))
    finally
      System.clearProperty(property)
      if !worker.isAlive then evaluator.shutdown()

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
      assertTrue(
        diagnostics.map(_.message).toString,
        diagnostics.exists(_.message.contains("unused"))
      )
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

  @Test def reportsCompilerOptionsTheReplRefuses(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-Ybest-effort"))
    try
      val result = evaluator.evaluate("options.worksheet.scala", "val x = 40\n")
      val messages = result.diagnostics().asScala.map(_.message()).toList
      assertTrue(messages.toString, messages.exists(_.contains("incompatible")))
    finally evaluator.shutdown()

  @Test def reportsCompilerOptionsThatAreNotRecognised(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-Wnosuchthing"))
    try
      val result = evaluator.evaluate("unknown.worksheet.scala", "val x = 40\n")
      assertEquals(1, result.statements().size)
      val diagnostic = result.diagnostics().get(0)
      assertEquals(interfaces.DiagnosticSeverity.Warning, diagnostic.severity())
      assertTrue(diagnostic.message(), diagnostic.message().contains("-Wnosuchthing"))
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
      assertEquals(interfaces.DiagnosticSeverity.Error, diagnostic.severity())
      assertTrue(diagnostic.message(), diagnostic.message().contains("not a valid choice"))
      assertTrue(diagnostic.message(), diagnostic.message().contains("Available choices"))
    finally evaluator.shutdown()

  @Test def keepsConfigurationDiagnosticsWhenTheTextDoesNotParse(): Unit =
    val evaluator = new WorksheetDriver()
      .withScalacOptions(java.util.List.of("-Ybest-effort"))
    try
      val messages = evaluator
        .evaluate("unparseable.worksheet.scala", "val x = (\n")
        .diagnostics().asScala.map(_.message()).toList
      assertTrue(messages.toString, messages.exists(_.contains("incompatible")))
      assertTrue(messages.toString, messages.exists(_.contains("expected")))
    finally evaluator.shutdown()
