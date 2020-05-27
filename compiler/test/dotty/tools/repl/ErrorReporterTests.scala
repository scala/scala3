package dotty.tools.repl

import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import java.nio.file.{Path, Files}
import java.util.Comparator
import java.util.regex.Pattern

import org.junit.{Test, BeforeClass, AfterClass}
import org.junit.Assert.assertEquals

class ErrorReporterTests:

  @Test def testCompilerError =
    val (output, errors) = eval("asdf")
    assertEquals("", output)
    assertEquals(List("Not found: asdf"), errors.map(_.msg.message))

  @Test def testParserError =
    val (output, errors) = eval("\"asdf")
    assertEquals("", output)
    assertEquals(List("Illegal start of statement", "unclosed string literal"), errors.map(_.msg.message))

  @Test def testReplError =
    val (output, errors) = eval(":load /home/rethab/nope/foo.scala")
    assertEquals("", output)
    assertEquals(List("Couldn't find file \"/home/rethab/nope/foo.scala\""), errors.map(_.msg.message))

  @Test def testWarning =
    val (output, errors) = eval("Option(42) match { case Some(x) => println(x) } ")
    assertEquals("42", output)
    assertEquals(List("match may not be exhaustive.\n\nIt would fail on pattern case: None"), errors.map(_.msg.message))

  @Test def tabCompletionError =
    val (completions, errors) = evalCompletions("List.r")
    assertEquals(List("range"), completions)
    assertEquals(List.empty, errors)

  private def eval(code: String): (String, List[Diagnostic]) =
    var errors: List[Diagnostic] = List.empty
    val reporter = new Reporter {
      override def doReport(dia: Diagnostic)(implicit ctx: Context): Unit =
        errors = dia :: errors
    }

    val driver = new ReplTest(errorReporter = Some(() => reporter))
    val output = driver.fromInitialState { implicit s =>
      driver.run(code)
      driver.storedOutput().trim
    }
    (output, errors)

  private def evalCompletions(code: String): (List[String], List[Diagnostic]) =
    var errors: List[Diagnostic] = List.empty
    val reporter = new Reporter {
      override def doReport(dia: Diagnostic)(implicit ctx: Context): Unit =
        errors = dia :: errors
    }

    val driver: ReplTest & RunCompletions = new ReplTest(errorReporter = Some(() => reporter)) with RunCompletions {
      def runCompletions(src: String)(implicit state: State): List[String] =
        completions(src.length, src, state).map(_.value).sorted
    }
    val cs = driver.fromInitialState { implicit s =>
      driver.runCompletions(code)
    }
    (cs, errors)

  // make protected method `completions` accessible
  trait RunCompletions {
    def runCompletions(src: String)(implicit state: State): List[String]
  }
