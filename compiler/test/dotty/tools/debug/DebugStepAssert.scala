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
        case eval(expr) :: resOrError :: tail =>
          val expected = parseResultOrError(resOrError)
          val step = DebugStepAssert(Eval(expr), checkEval(expected))
          loop(tail, step :: acc)
        case multiLineEval() :: tail0 =>
          val (exprLines, tail1) = tail0.span(_.startsWith("  "))
          val expr = exprLines.map(s => s.stripPrefix("  ")).mkString("\n")
          val expected = parseResultOrError(tail1.head)
          val step = DebugStepAssert(Eval(expr), checkEval(expected))
          loop(tail1.tail, step :: acc)
        case trailing() :: tail => loop(tail, acc)
        case invalid :: tail => throw new Exception(s"Cannot parse debug step: $invalid")

    def parseResultOrError(line: String): Either[String, String] =
      line match
        case result(expected) => Right(expected)
        case error(expected) => Left(expected)
        case invalid => throw new Exception(s"Cannot parse as result or error: $invalid")

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

  private def checkEval(expected: Either[String, String])(obtained: Either[String, String]): Unit =
    (expected, obtained) match
      case (Left(expected), Left(obtained)) => assert(expected.r.matches(obtained), s"obtained $obtained, expected $expected")
      case (Right(expected), Right(obtained)) => assert(expected.r.matches(obtained.toString), s"obtained $obtained, expected $expected")
      case (Left(_), Right(_)) => throw new AssertionError("evaluation succeeded but error expected")
      case (Right(_), Left(_)) => throw new AssertionError("evaluation failed")

end DebugStepAssert

private[debug] enum DebugStep[T]:
  case Break(className: String, line: Int) extends DebugStep[Location]
  case Step extends DebugStep[Location]
  case Next extends DebugStep[Location]
  case Eval(expression: String) extends DebugStep[Either[String, String]]



