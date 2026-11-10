package dotty.tools.debug

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.Driver

// IMPORTANT: This is a public class used externally, e.g., scala-debug-adapter instantiates it
//            and calls `run` via reflection!
class ExpressionCompilerBridge:
  def run(
      outputDir: java.nio.file.Path, // TODO: make this AnyRef to avoid nio and remain compatible since all we do is .toString on it?
      classPath: String,
      options: Array[String],
      sourceFile: java.nio.file.Path, // TODO: ditto
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
      case cause: Exception =>
        cause.printStackTrace()
        throw cause
