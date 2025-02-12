package dotty.tools.debug

import scala.util.Using
import scala.io.Source
import java.nio.charset.StandardCharsets
import scala.io.Codec
import dotty.tools.io.JFile
import java.nio.file.Files
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
    val break = s"break ($sym) ($line)".r
    val step = s"step ($sym|$line)".r
    val next = s"next ($sym|$line)".r
    val comment = "// .*".r
    val empty = "\\w*".r
    readLines(checkFile).flatMap:
      case break(className , lineStr) =>
        val line = lineStr.toInt
        Some(DebugStepAssert(Break(className, line), checkFrame(className, line)))
      case step(pattern) => Some(DebugStepAssert(Step, checkLineOrMethod(pattern)))
      case next(pattern) => Some(DebugStepAssert(Step, checkLineOrMethod(pattern)))
      case comment() | empty() => None
      case invalid => throw new Exception(s"Cannot parse debug step: $invalid")

  private def checkFrame(className: String, line: Int)(frame: Frame): Unit =
    assert(className.matches(frame.className))
    assert(frame.line == line)

  private def checkLineOrMethod(pattern: String): Frame => Unit =
    if "(\\d+)".r.matches(pattern) then checkLine(pattern.toInt) else checkMethod(pattern)

  private def checkLine(line: Int)(frame: Frame): Unit = assert(frame.line == line)

  private def checkMethod(method: String)(frame: Frame): Unit =
    assert(method.matches(s"${frame.className}.${frame.methodName}"))
end DebugStepAssert

private[debug] enum DebugStep[T]:
  case Break(className: String, line: Int) extends DebugStep[Frame]
  case Step extends DebugStep[Frame]
  case Next extends DebugStep[Frame]

private[debug] case class Frame(className: String, methodName: String, line: Int)


