package dotty.tools.debug

import java.nio.file.Path
import scala.util.control.NonFatal
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.Driver

class ExpressionCompilerBridge:
  def run(
      outputDir: Path,
      classPath: String,
      options: Array[String],
      sourceFile: Path,
      config: ExpressionCompilerConfig
  ): Boolean =
    val args = Array(
      "-d",
      outputDir.toString,
      "-classpath",
      classPath,
      "-Yskip:pureStats"
      // Debugging: Print the tree after phases of the debugger
      // "-Vprint:insert-expression,resolve-reflect-eval",
    ) ++ options :+ sourceFile.toString
    val driver = new Driver:
      protected override def newCompiler(using Context): ExpressionCompiler = ExpressionCompiler(config)
    val reporter = ExpressionReporter(error => config.errorReporter.accept(error))
    try
      driver.process(args, reporter)
      !reporter.hasErrors
    catch
      case NonFatal(cause) =>
        cause.printStackTrace()
        throw cause
