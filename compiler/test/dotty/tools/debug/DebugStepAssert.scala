package dotty.tools.debug

import com.sun.jdi.Location
import dotty.tools.io.JPath
import dotty.tools.readLines

import scala.annotation.tailrec

/**
 * A debug step and an associated assertion to validate the step.
 * A sequence of DebugStepAssert is parsed from the check file in tests/debug
 */
private[debug] case class DebugStepAssert[T](step: DebugStep[T], assertion: T => Unit)(
  using val location: CheckFileLocation
)

/** A location in the check file */
private[debug] case class CheckFileLocation(checkFile: JPath, line: Int):
  override def toString: String = s"$checkFile:$line"

/** When a DebugStepAssert fails it throws a DebugStepException */
private[debug] case class DebugStepException(message: String, location: CheckFileLocation) extends Exception

private[debug] enum DebugStep[T]:
  case Break(className: String, line: Int) extends DebugStep[Location]
  case Step extends DebugStep[Location]
  case Next extends DebugStep[Location]
  case Eval(expression: String) extends DebugStep[Either[String, String]]

private[debug] object DebugStepAssert:
  private val sym = "[a-zA-Z0-9$.]+"
  private val line = raw"\d+"
  private val trailing = raw" *(?://.*)?".r // empty or comment
  private val break = s"break ($sym) ($line)$trailing".r
  private val step = s"step ($sym|$line)$trailing".r
  private val next = s"next ($sym|$line)$trailing".r
  private val multiLineEval = s"eval$trailing".r
  private val eval = s"eval (.*)".r
  private val result = "result (.*)".r
  private val error = "error (.*)".r
  private val multiLineError = s"error$trailing".r

  import DebugStep.*
  def parseCheckFile(checkFile: JPath): Seq[DebugStepAssert[?]] =
    val allLines = readLines(checkFile.toFile)

    @tailrec
    def loop(lines: List[String], acc: List[DebugStepAssert[?]]): List[DebugStepAssert[?]] =
      given location: CheckFileLocation = CheckFileLocation(checkFile, allLines.size - lines.size + 1)
      lines match
        case Nil => acc.reverse
        case break(className: String, lineStr: String) :: tail =>
          val breakpointLine = lineStr.toInt
          val step = DebugStepAssert(Break(className, breakpointLine), checkClassAndLine(className, breakpointLine))
          loop(tail, step :: acc)
        case step(pattern: String) :: tail =>
          val step = DebugStepAssert(Step, checkLineOrMethod(pattern))
          loop(tail, step :: acc)
        case next(pattern: String) :: tail =>
          val step = DebugStepAssert(Next, checkLineOrMethod(pattern))
          loop(tail, step :: acc)
        case eval(expr: String) :: tail0 =>
          val (assertion, tail1) = parseEvalAssertion(tail0)
          val step = DebugStepAssert(Eval(expr), assertion)
          loop(tail1, step :: acc)
        case multiLineEval() :: tail0 =>
          val (exprLines, tail1) = tail0.span(_.startsWith("  "))
          val expr = exprLines.map(s => s.stripPrefix("  ")).mkString("\n")
          val (assertion, tail2) = parseEvalAssertion(tail1)
          val step = DebugStepAssert(Eval(expr), assertion)
          loop(tail2, step :: acc)
        case trailing() :: tail => loop(tail, acc)
        case invalid :: tail =>
          throw new Exception(s"Cannot parse debug step: $invalid ($location)")

    def parseEvalAssertion(lines: List[String]): (Either[String, String] => Unit, List[String]) =
      given location: CheckFileLocation = CheckFileLocation(checkFile, allLines.size - lines.size + 1)
      lines match
        case Nil => throw new Exception(s"Missing result or error")
        case trailing() :: tail => parseEvalAssertion(tail)
        case result(expected: String) :: tail => (checkResult(expected), tail)
        case error(expected: String) :: tail => (checkError(Seq(expected)), tail)
        case multiLineError() :: tail0 =>
          val (expected, tail1) = tail0.span(_.startsWith("  "))
          (checkError(expected.map(_.stripPrefix("  "))), tail1)
        case invalid :: _ =>
          throw new Exception(s"Cannot parse as result or error: $invalid ($location)")

    loop(allLines, Nil)
  end parseCheckFile

  private def checkClassAndLine(className: String, breakpointLine: Int)(using CheckFileLocation)(location: Location): Unit =
    debugStepAssertEquals(location.declaringType.name, className)
    checkLine(breakpointLine)(location)

  private def checkLineOrMethod(pattern: String)(using CheckFileLocation): Location => Unit =
    pattern.toIntOption.map(checkLine).getOrElse(checkMethod(pattern))

  private def checkLine(expected: Int)(using CheckFileLocation)(location: Location): Unit =
    debugStepAssertEquals(location.lineNumber, expected)

  private def checkMethod(expected: String)(using CheckFileLocation)(location: Location): Unit =
    debugStepAssertEquals(location.method.name, expected)

  private def checkResult(expected: String)(using CheckFileLocation)(obtained: Either[String, String]): Unit =
    obtained match
      case Left(obtained) =>
        debugStepFailed(
          s"""|Evaluation failed:
              |${obtained.replace("\n", "\n|")}""".stripMargin
        )
      case Right(obtained) => debugStepAssertEquals(obtained, expected)

  private def checkError(expected: Seq[String])(using CheckFileLocation)(obtained: Either[String, String]): Unit =
    obtained match
      case Left(obtained) =>
        debugStepAssert(
          expected.forall(e => e.r.findFirstMatchIn(obtained).isDefined),
          s"""|Expected:
              |${expected.mkString("\n|")}
              |Obtained:
              |${obtained.replace("\n", "\n|")}""".stripMargin
        )
      case Right(obtained) =>
        debugStepFailed(
          s"""|Evaluation succeeded but failure expected.
              |Obtained: $obtained
              |""".stripMargin
        )

  private def debugStepAssertEquals[T](obtained: T, expected: T)(using CheckFileLocation): Unit =
    debugStepAssert(obtained == expected, s"Obtained $obtained, Expected: $expected")

  private def debugStepAssert(assertion: Boolean, message: String)(using CheckFileLocation): Unit =
    if !assertion then debugStepFailed(message)

  private def debugStepFailed(message: String)(using location: CheckFileLocation): Unit =
    throw DebugStepException(message, location)
end DebugStepAssert
