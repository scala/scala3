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
    readLines(checkFile).flatMap:
      case break(className , lineStr) =>
        val line = lineStr.toInt
        Some(DebugStepAssert(Break(className, line), checkClassAndLine(className, line)))
      case step(pattern) => Some(DebugStepAssert(Step, checkLineOrMethod(pattern)))
      case next(pattern) => Some(DebugStepAssert(Next, checkLineOrMethod(pattern)))
      case trailing() => None
      case invalid => throw new Exception(s"Cannot parse debug step: $invalid")

  private def checkClassAndLine(className: String, line: Int)(location: Location): Unit =
    assert(className == location.declaringType.name, s"obtained ${location.declaringType.name}, expected ${className}")
    checkLine(line)(location)

  private def checkLineOrMethod(pattern: String): Location => Unit =
    if "(\\d+)".r.matches(pattern) then checkLine(pattern.toInt) else checkMethod(pattern)

  private def checkLine(line: Int)(location: Location): Unit =
    assert(location.lineNumber == line, s"obtained ${location.lineNumber}, expected $line")

  private def checkMethod(methodName: String)(location: Location): Unit = assert(methodName == location.method.name)
end DebugStepAssert

private[debug] enum DebugStep[T]:
  case Break(className: String, line: Int) extends DebugStep[Location]
  case Step extends DebugStep[Location]
  case Next extends DebugStep[Location]



