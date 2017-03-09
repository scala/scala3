package dotty
package tools
package dotc

import java.io.{ File => JFile }
import scala.io.Source

import core.Contexts._
import reporting.{ Reporter, UniqueMessagePositions, HideNonSensicalMessages, MessageRendering }
import reporting.diagnostic.MessageContainer
import interfaces.Diagnostic.ERROR
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{ Files, Path, Paths }

trait ParallelTesting {

  private val driver = new Driver {
    override def newCompiler(implicit ctx: Context) = new Compiler
  }

  private class DaftReporter(suppress: Boolean)
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages
  with MessageRendering {
    private var _errors: List[MessageContainer] = Nil
    def errors = _errors

    override def doReport(m: MessageContainer)(implicit ctx: Context) = {
      if (!suppress && m.level == ERROR) {
        _errors = m :: _errors
        System.err.println(messageAndPos(m.contained, m.pos, diagnosticLevel(m)))
      }
    }
  }

  private def compile(files: Array[JFile], flags: Array[String]): (Array[JFile], List[MessageContainer]) = {
    val reporter = new DaftReporter(suppress = false)
    driver.process(flags ++ files.map(_.getAbsolutePath), reporter = reporter)
    files -> reporter.errors
  }

}
