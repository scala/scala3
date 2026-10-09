package dotty.tools.repl.worksheet

import dotty.tools.repl.worksheet.WorksheetChecks.*

import org.junit.Assert.*
import org.junit.Test

class WorksheetEvaluationTest extends WorksheetTest:
  private def checkStatements(text: String, expected: (String, String)*): WorksheetResult =
    val result = driver.evaluate("evaluation.worksheet.scala", text)
    checkDiagnostics(result)
    val expectedStatements = expected.toList.map: (source, details) =>
      val start = text.indexOf(source)
      require(start >= 0, s"Expected statement is missing from the test source: $source")
      s"[$start..${start + source.length}]\n$details"
    val obtained = result.statements.map: statement =>
      val position = statement.position
      s"[${position.start}..${position.end}]\n${statement.details}"
    checkText(
      obtained.mkString("\n\n"),
      expectedStatements.mkString("\n\n"),
      s"Worksheet statement ranges and details:\n$text"
    )
    result

  @Test def defineObject(): Unit =
    checkStatements(
      "def foo(x: Int) = x + 1\nfoo(1)\n",
      "foo(1)" -> "res0: Int = 2"
    )

  @Test def defineCaseClass(): Unit =
    checkStatements(
      "case class Foo(x: Int)\nFoo(1)\n",
      "Foo(1)" -> "res0: Foo = Foo(1)"
    )

  @Test def defineClass(): Unit =
    val definition =
      """class Foo(x: Int) {
        |  override def toString: String = "Foo"
        |}""".stripMargin
    checkStatements(
      s"$definition\nnew Foo(1)\n",
      "new Foo(1)" -> "res0: Foo = Foo"
    )

  @Test def defineAnonymousClass0(): Unit =
    val text =
      """new {
        |  override def toString: String = "Foo"
        |}""".stripMargin
    checkStatements(text, text -> "res0: Object = Foo")

  @Test def defineAnonymousClass1(): Unit =
    val expression =
      """new Foo with Bar {
        |  override def toString: String = "Foo"
        |}""".stripMargin
    checkStatements(
      s"class Foo\ntrait Bar\n$expression\n",
      expression -> "res0: Foo & Bar = Foo"
    )

  @Test def produceMultilineOutput(): Unit =
    val text = "1 to 3 foreach println"
    val result = checkStatements(text, text -> "// 1\n// 2\n// 3")
    checkSummaries(result, "1" -> false)

  @Test def patternMatching0(): Unit =
    val text =
      """1 + 2 match {
        |  case x if x % 2 == 0 => "even"
        |  case _ => "odd"
        |}""".stripMargin
    checkStatements(text, text -> "res0: String = \"odd\"")

  @Test def evaluationException(): Unit =
    val text = "val foo = 1 / 0\nval bar = 2\n"
    val result = driver.evaluate("exception.worksheet.scala", text)
    assertEquals(
      List(("val foo = 1 / 0", "java.lang.ArithmeticException: / by zero", WorksheetDiagnosticSeverity.Error)),
      result.diagnostics.map: diagnostic =>
        val position = diagnostic.position
        (text.slice(position.start, position.end), diagnostic.message, diagnostic.severity)
    )
    checkDetails(result)
