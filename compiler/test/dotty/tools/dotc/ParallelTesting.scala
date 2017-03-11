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
import java.util.concurrent.{ Executors => JExecutors, TimeUnit }
import scala.util.control.NonFatal

trait ParallelTesting {

  private abstract class CompileRun(targetDirs: List[JFile], fromDir: String, flags: Array[String]) {
    val totalTargets = targetDirs.length

    private[this] var _errors =  0
    def errors: Int = synchronized { _errors }

    private[this] var _targetsCompiled = 0
    private def targetsCompiled: Int = synchronized { _targetsCompiled }

    protected final def completeCompilation(newErrors: Int) = synchronized {
      _targetsCompiled += 1
      _errors += newErrors
    }

    private def statusRunner: Runnable = new Runnable {
      def run(): Unit = {
        val start = System.currentTimeMillis
        var tCompiled = targetsCompiled
        while (tCompiled < totalTargets) {
          val timestamp = (System.currentTimeMillis - start) / 1000
          val progress = (tCompiled.toDouble / totalTargets * 40).toInt
          print(
            s"Compiling tests in $fromDir [" +
            ("=" * (math.max(progress - 1, 0))) +
            (if (progress > 0) ">" else "") +
            (" " * (39 - progress)) +
            s"] $tCompiled/$totalTargets, ${timestamp}s, errors: $errors\r"
          )
          Thread.sleep(50)
          tCompiled = targetsCompiled
        }
        // println, otherwise no newline and cursor at start of line
        println(
          s"Compiled tests in $fromDir " +
          s"[========================================] $totalTargets/$totalTargets, " +
          s"${(System.currentTimeMillis - start) / 1000}s, errors: $errors  "
        )
      }
    }

    protected def compilationRunnable(dir: JFile): Runnable

    private[ParallelTesting] def execute(): this.type = {
      assert(_targetsCompiled == 0, "not allowed to re-use a `CompileRun`")
      val pool = JExecutors.newWorkStealingPool()
      pool.submit(statusRunner)

      targetDirs.foreach { dir =>
        pool.submit(compilationRunnable(dir))
      }

      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.MINUTES)
      this
    }
  }

  private final class PosCompileRun(targetDirs: List[JFile], fromDir: String, flags: Array[String])
  extends CompileRun(targetDirs, fromDir, flags) {
    protected def compilationRunnable(dir: JFile): Runnable = new Runnable {
      def run(): Unit =
        try {
          val sourceFiles = dir.listFiles.filter(f => f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
          val errors = compile(sourceFiles, flags ++ Array("-d", dir.getAbsolutePath), false)
          completeCompilation(errors.length)
        }
        catch {
          case NonFatal(e) => {
            System.err.println(s"\n${e.getMessage}\n")
            completeCompilation(1)
            throw e
          }
        }
    }
  }

  private final class NegCompileRun(targetDirs: List[JFile], fromDir: String, flags: Array[String])
  extends CompileRun(targetDirs, fromDir, flags) {
    private[this] var _failed = false
    private[this] def fail(): Unit = _failed = true

    def didFail: Boolean = _failed

    protected def compilationRunnable(dir: JFile): Runnable = new Runnable {
      def run(): Unit =
        try {
          val sourceFiles = dir.listFiles.filter(f => f.getName.endsWith(".scala") || f.getName.endsWith(".java"))

          val expectedErrors = dir.listFiles.filter(_.getName.endsWith(".scala")).foldLeft(0) { (acc, file) =>
            acc + Source.fromFile(file).sliding("// error".length).count(_.mkString == "// error")
          }

          val errors = compile(sourceFiles, flags ++ Array("-d", dir.getAbsolutePath), true)
          val actualErrors = errors.length

          if (expectedErrors != actualErrors) {
            System.err.println {
              s"\nWrong number of errors encountered when compiling $dir, expected: $expectedErrors, actual: $actualErrors\n"
            }
            fail()
          }

          completeCompilation(actualErrors)
        }
        catch {
          case NonFatal(e) => {
            System.err.println(s"\n${e.getMessage}\n")
            completeCompilation(1)
            throw e
          }
        }
    }
  }

  private val driver = new Driver {
    override def newCompiler(implicit ctx: Context) = new Compiler
  }

  private class DaftReporter(suppress: Boolean)
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages
  with MessageRendering {
    private var _errors: List[MessageContainer] = Nil
    def errors = _errors

    override def doReport(m: MessageContainer)(implicit ctx: Context) = {
      if (m.level == ERROR) {
        _errors = m :: _errors
        if (!suppress) System.err.println(messageAndPos(m.contained, m.pos, diagnosticLevel(m)))
      }
    }
  }

  private def compile(files: Array[JFile], flags: Array[String], suppressErrors: Boolean): List[MessageContainer] = {

    def findJarFromRuntime(partialName: String) = {
      val urls = ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile.toString)
      urls.find(_.contains(partialName)).getOrElse {
        throw new java.io.FileNotFoundException(
          s"""Unable to locate $partialName on classpath:\n${urls.toList.mkString("\n")}"""
        )
      }
    }

    def compileWithJavac(fs: Array[String]) = if (fs.nonEmpty) {
      val scalaLib = findJarFromRuntime("scala-library")
      val fullArgs = Array(
        "javac",
        "-classpath",
        s".:$scalaLib"
      ) ++ flags.takeRight(2) ++ fs

      assert(Runtime.getRuntime.exec(fullArgs).waitFor() == 0, s"java compilation failed for ${fs.mkString(", ")}")
    }

    compileWithJavac(files.filter(_.getName.endsWith(".java")).map(_.getAbsolutePath))

    val reporter = new DaftReporter(suppress = suppressErrors)
    driver.process(flags ++ files.map(_.getAbsolutePath), reporter = reporter)
    reporter.errors
  }


  class CompilationTest(targetDirs: List[JFile], fromDir: String, flags: Array[String]) {
    def pos: Unit = {
      val run = new PosCompileRun(targetDirs, fromDir, flags).execute()
      assert(run.errors == 0, s"Expected no errors when compiling $fromDir")
    }

    def neg: Unit = assert(
      !(new NegCompileRun(targetDirs, fromDir, flags).execute().didFail),
      s"Wrong number of errors encountered when compiling $fromDir"
    )
  }

  def compileFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"

    val dir = new JFile(f)
    require(f.contains("/tests"), "only allowed to run integration tests from `tests` dir using this method")
    require(dir.isDirectory && dir.exists, "passed non-directory to `compileFilesInDir`")
    require(outDir.last == '/', "please specify an `outDir` with a trailing slash")

    def toCompilerDirFromDir(d: JFile): JFile = {
      val targetDir = new JFile(outDir + s"${dir.getName}/${d.getName}")
      // create if not exists
      targetDir.mkdirs()
      d.listFiles.foreach(copyToDir(targetDir, _))
      targetDir
    }
    def toCompilerDirFromFile(file: JFile): JFile = {
      val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
      val targetDir = new JFile(outDir + s"${dir.getName}/$uniqueSubdir")
      // create if not exists
      targetDir.mkdirs()
      // copy file to dir:
      copyToDir(targetDir, file)
      targetDir
    }
    def copyToDir(dir: JFile, file: JFile): Unit = {
      val target = Paths.get(dir.getAbsolutePath, file.getName)
      Files.copy(file.toPath, target, REPLACE_EXISTING).toFile
    }

    val (dirs, files) =
      dir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
        if (f.isDirectory) (f :: dirs, files)
        else (dirs, f :: files)
      }

    // Directories in which to compile all containing files with `flags`:
    val dirsToCompile = files.map(toCompilerDirFromFile) ++ dirs.map(toCompilerDirFromDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(dirsToCompile, f, flags)
  }
}
