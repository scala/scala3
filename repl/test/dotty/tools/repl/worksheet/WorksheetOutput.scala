package dotty.tools.repl.worksheet

import scala.jdk.CollectionConverters.*

private[worksheet] object WorksheetOutput:
  extension (result: WorksheetResult)
    def rendered: String = render(result.statements.map(one => (one.summary, one.details)))

  extension (evaluated: interfaces.EvaluatedWorksheet)
    def rendered: String =
      render(evaluated.statements.asScala.toList.map(one => (one.summary, one.details)))

  private def render(statements: List[(String, String)]): String =
    statements.map((summary, details) => s"$summary\n$details").mkString("\n\n")
