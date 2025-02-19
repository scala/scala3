package dotty.tools.debug

import com.sun.jdi.Location
import dotty.tools.io.JFile
import dotty.tools.readLines

/**
 * A debug step and an associated assertion to validate the step.
 * A sequence of DebugStepAssert is parsed from the check file in tests/debug
 */
private[debug] case class DebugStepAssert[T](step: DebugStep[T], assert: T => Unit)

private[debug] object DebugStepAssert:
  import DebugStep.*
  def parseCheckFile(checkFile: JFile): Seq[DebugStepAssert[?]] =
    val sym = "[a-zA-Z0-9$.]+"
    val line = "\\d+"
    val trailing = s"\\s*(?:\\/\\/.*)?".r // empty or comment
    val break = s"break ($sym) ($line)$trailing".r
    val step = s"step ($sym|$line)$trailing".r
    val next = s"next ($sym|$line)$trailing".r
    val multiLineEval = s"eval$trailing".r
    val eval = s"eval (.*)".r
    val result = "result (.*)".r
    val error = "error (.*)".r
    val multiLineError = s"error$trailing".r

    def loop(lines: List[String], acc: List[DebugStepAssert[?]]): List[DebugStepAssert[?]] =
      lines match
        case Nil => acc.reverse
        case break(className , lineStr) :: tail =>
          val line = lineStr.toInt
          val step = DebugStepAssert(Break(className, line), checkClassAndLine(className, line))
          loop(tail, step :: acc)
        case step(pattern) :: tail =>
          val step = DebugStepAssert(Step, checkLineOrMethod(pattern))
          loop(tail, step :: acc)
        case next(pattern) :: tail =>
          val step = DebugStepAssert(Next, checkLineOrMethod(pattern))
          loop(tail, step :: acc)
        case eval(expr) :: tail0 =>
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
        case invalid :: tail => throw new Exception(s"Cannot parse debug step: $invalid")

    def parseEvalAssertion(lines: List[String]): (Either[String, String] => Unit, List[String]) =
      lines match
        case Nil => throw new Exception(s"Missing result or error")
        case result(expected) :: tail => (checkResult(expected), tail)
        case error(expected) :: tail => (checkError(Seq(expected)), tail)
        case multiLineError() :: tail0 =>
          val (expected, tail1) = tail0.span(_.startsWith("  "))
          (checkError(expected.map(_.stripPrefix("  "))), tail1)
        case invalid :: _ => throw new Exception(s"Cannot parse as result or error: $invalid")

    loop(readLines(checkFile), Nil)
  end parseCheckFile

  private def checkClassAndLine(className: String, line: Int)(location: Location): Unit =
    assert(className == location.declaringType.name, s"obtained ${location.declaringType.name}, expected ${className}")
    checkLine(line)(location)

  private def checkLineOrMethod(pattern: String): Location => Unit =
    if "(\\d+)".r.matches(pattern) then checkLine(pattern.toInt) else checkMethod(pattern)

  private def checkLine(line: Int)(location: Location): Unit =
    assert(location.lineNumber == line, s"obtained ${location.lineNumber}, expected $line")

  private def checkMethod(methodName: String)(location: Location): Unit = assert(methodName == location.method.name)

  private def checkResult(expected: String)(obtained: Either[String, String]): Unit =
    obtained match
      case Left(obtained) =>
        val message =
          s"""|Evaluation failed:
              |${obtained.replace("\n", "\n|")}""".stripMargin
        throw new AssertionError(message)
      case Right(obtained) =>
        val message =
          s"""|Expected: $expected
              |Obtained: $obtained""".stripMargin
        assert(expected.r.matches(obtained.toString), message)

  private def checkError(expected: Seq[String])(obtained: Either[String, String]): Unit =
    obtained match
      case Left(obtained) =>
        val message =
          s"""|Expected:
              |${expected.mkString("\n")}
              |Obtained:
              |${obtained.replace("\n", "\n|")}""".stripMargin
        assert(expected.forall(e => e.r.findFirstMatchIn(obtained).isDefined), message)
      case Right(obtained) =>
        val message =
          s"""|Evaluation succeeded but failure expected.
              |Obtained: $obtained
              |""".stripMargin
        throw new AssertionError(message)


end DebugStepAssert

private[debug] enum DebugStep[T]:
  case Break(className: String, line: Int) extends DebugStep[Location]
  case Step extends DebugStep[Location]
  case Next extends DebugStep[Location]
  case Eval(expression: String) extends DebugStep[Either[String, String]]



