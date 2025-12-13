package scala.tasty.interpreter

import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.reporting.*
import dotty.tools.io.*
import dotty.tools.dotc.core.Comments.{ContextDoc, ContextDocstrings}

import scala.collection.mutable

/**
 * Pure Scala Compiler for browser-based execution.
 *
 * This compiler compiles Scala source code to TASTy format and can execute
 * the generated TASTy directly using a tree interpreter.
 *
 * Key characteristics:
 * - No macro support (macros are not expanded)
 * - No backend code generation (no bytecode or JS IR)
 * - Direct TASTy execution via tree interpretation
 * - Self-contained (all dependencies bundled)
 */
class PureScalaCompiler {

  /**
   * Compilation result containing either TASTy bytes or compilation errors.
   */
  sealed trait CompilationResult
  case class CompilationSuccess(tastyBytes: Array[Byte]) extends CompilationResult
  case class CompilationFailure(errors: List[String]) extends CompilationResult

  /**
   * Execution result containing output and return value.
   */
  case class ExecutionResult(
    output: String,
    returnValue: Option[Any] = None
  )

  private val virtualStdlibDir = new VirtualDirectory("stdlib")
  private val virtualOutputDir = new VirtualDirectory("output")
  private val contextBase = new ContextBase

  /**
   * Initialize the compiler with standard library TASTy files.
   *
   * @param stdlibTastyBundle Map from class name (e.g., "scala/String") to TASTy bytes
   */
  def initialize(stdlibTastyBundle: Map[String, Array[Byte]]): Unit = {
    // Load stdlib TASTy files into virtual filesystem
    for ((className, bytes) <- stdlibTastyBundle) {
      val pathParts = (className.replace('.', '/') + ".tasty").split('/').toList
      val fileName = pathParts.last
      val dirPath = pathParts.init

      // Create nested directories if needed
      var currentDir = virtualStdlibDir
      for (dirName <- dirPath) {
        val subDir = currentDir.lookupName(dirName, directory = true)
        currentDir = if (subDir != null) subDir.asInstanceOf[VirtualDirectory]
        else currentDir.subdirectoryNamed(dirName).asInstanceOf[VirtualDirectory]
      }

      // Create the file using VirtualFile constructor that takes path and content
      val fullPath = (dirPath :+ fileName).mkString("/")
      val file = new VirtualFile(fullPath, bytes)
      // Note: VirtualDirectory doesn't have a direct addFile method,
      // but we can use fileNamed which creates or returns existing file
      // For now, we'll store files directly - this may need adjustment
      val createdFile = currentDir.fileNamed(fileName)
      val output = createdFile.output
      output.write(bytes)
      output.close()
    }
  }

  /**
   * Compile Scala source code to TASTy format.
   *
   * @param sourceCode The Scala source code to compile
   * @param sourceName Optional name for the source file (defaults to "Main.scala")
   * @return CompilationResult with either TASTy bytes or errors
   */
  def compile(sourceCode: String, sourceName: String = "Main.scala"): CompilationResult = {
    val virtualSource = new VirtualFile(sourceName, sourceCode.getBytes("UTF-8"))

    val rootCtx = contextBase.initialCtx.fresh
    rootCtx.setSetting(rootCtx.settings.outputDir, virtualOutputDir)
    rootCtx.setSetting(rootCtx.settings.classpath, virtualStdlibDir.path)
    rootCtx.setSetting(rootCtx.settings.YretainTrees, true) // Retain trees for interpretation
    rootCtx.setProperty(ContextDoc, new ContextDocstrings)

    // Initialize the context base with our settings
    contextBase.initialize()(using rootCtx)

    val compiler = new Compiler()
    val run = compiler.newRun(using rootCtx)

    try {
      run.compile(List(virtualSource))

      if (rootCtx.reporter.hasErrors) {
        val errors = collectErrors(rootCtx.reporter)
        CompilationFailure(errors)
      } else {
        // Extract generated TASTy
        val tastyFile = virtualOutputDir.iterator
          .find(_.name.endsWith(".tasty"))
          .getOrElse(throw new Exception("No TASTy file generated"))

        CompilationSuccess(tastyFile.toByteArray)
      }
    } catch {
      case e: Exception =>
        CompilationFailure(List(s"Compilation failed: ${e.getMessage}"))
    }
  }

  /**
   * Execute TASTy bytes directly using tree interpretation.
   *
   * This method unpickles the TASTy and executes it using the tree interpreter.
   *
   * @param tastyBytes The TASTy bytes to execute
   * @return ExecutionResult with output and return value
   */
  def execute(tastyBytes: Array[Byte]): ExecutionResult = {
    import scala.tasty.interpreter.pure.PureInterpreterInspector
    import scala.tasty.inspector.TastyInspector

    // Write TASTy bytes to virtual file for inspector
    val tastyFile = virtualOutputDir.fileNamed("Main.tasty")
    val output = tastyFile.output
    output.write(tastyBytes)
    output.close()

    val stdout = new java.io.ByteArrayOutputStream()
    try {
      scala.Console.withOut(stdout) {
        // Use TASTy Inspector to interpret
        TastyInspector.inspectTastyFiles(List(tastyFile.path))(new PureInterpreterInspector)
      }
      ExecutionResult(
        output = filterDiagnosticOutput(stdout.toString),
        returnValue = None
      )
    } catch {
      case e: Exception =>
        ExecutionResult(
          output = s"Execution error: ${e.getMessage}\nOutput so far: ${stdout.toString}",
          returnValue = None
        )
    }
  }

  /**
   * Filter out diagnostic lines from interpreter output.
   */
  private def filterDiagnosticOutput(output: String): String = {
    output.linesIterator
      .filterNot(_.startsWith("[PureInterpreter]"))
      .mkString("\n")
  }

  /**
   * Compile and execute Scala source code in one step.
   *
   * @param sourceCode The Scala source code
   * @return Either ExecutionResult on success, or error messages on failure
   */
  def compileAndExecute(sourceCode: String): Either[List[String], ExecutionResult] = {
    compile(sourceCode) match {
      case CompilationSuccess(tastyBytes) =>
        Right(execute(tastyBytes))
      case CompilationFailure(errors) =>
        Left(errors)
    }
  }

  /**
   * Collect error messages from the reporter.
   */
  private def collectErrors(reporter: Reporter): List[String] = {
    val errors = mutable.ListBuffer[String]()
    reporter match {
      case storeReporter: StoreReporter =>
        // StoreReporter stores messages that can be accessed
        // For now, return a simple message - will be enhanced later
        errors += "Compilation errors occurred (error details not yet extracted)"
      case _ =>
        errors += "Compilation failed"
    }
    errors.toList
  }
}

