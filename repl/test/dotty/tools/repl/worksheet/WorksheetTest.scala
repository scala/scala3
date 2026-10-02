package dotty.tools.repl.worksheet

import dotty.tools.dotc.util.DiffUtil
import dotty.tools.repl.ReplTest
import dotty.tools.repl.worksheet.WorksheetOutput.rendered

import org.junit.After
import org.junit.ComparisonFailure

import scala.jdk.CollectionConverters.*

private[worksheet] object WorksheetChecks:
  private type Result = WorksheetResult | interfaces.EvaluatedWorksheet

  def checkText(obtained: String, expected: String, hint: String): Unit =
    if obtained != expected then
      throw difference(obtained, expected, hint)

  private def difference(obtained: String, expected: String, hint: String): ComparisonFailure =
    val diff = DiffUtil.mkColoredHorizontalLineDiff(expected, obtained)
    new ComparisonFailure(s"$hint\n$diff", expected, obtained)

  private def checkEntries(obtained: List[String], expected: Seq[String], hint: String): Unit =
    if obtained != expected.toList then
      def render(entries: Seq[String]): String =
        entries.zipWithIndex.map((entry, index) => s"[$index]\n$entry").mkString("\n\n")
      throw difference(render(obtained), render(expected), hint)

  def checkDiagnostics(result: Result, expected: String*): Unit =
    val messages = result match
      case result: WorksheetResult => result.diagnostics.map(_.message)
      case result: interfaces.EvaluatedWorksheet => result.diagnostics().asScala.toList.map(_.message())
    checkEntries(messages, expected, "Worksheet diagnostics")

  def checkDetails(result: Result, expected: String*): Unit =
    val details = result match
      case result: WorksheetResult => result.statements.map(_.details)
      case result: interfaces.EvaluatedWorksheet => result.statements().asScala.toList.map(_.details())
    checkEntries(details, expected, "Worksheet statement details")

  def checkSummaries(result: WorksheetResult, expected: (String, Boolean)*): Unit =
    def render(summary: String, complete: Boolean): String =
      s"summary: $summary\ncomplete: $complete"
    checkEntries(
      result.statements.map(statement => render(statement.summary, statement.isSummaryComplete)),
      expected.map((summary, complete) => render(summary, complete)),
      "Worksheet summaries"
    )

  def checkOutput(result: Result, expected: String, diagnostics: String*): Unit =
    checkDiagnostics(result, diagnostics*)
    val output = result match
      case result: WorksheetResult => result.rendered
      case result: interfaces.EvaluatedWorksheet => result.rendered
    checkText(output, expected, "Worksheet output")

private[worksheet] abstract class WorksheetTest:
  protected val driver = new WorksheetSession(ReplTest.defaultOptions)

  @After def shutdownDriver(): Unit = driver.shutdown()

  protected def check(filename: String, text: String, expected: String, diagnostics: String*): Unit =
    WorksheetChecks.checkOutput(driver.evaluate(filename, text), expected, diagnostics*)
