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
import java.util.HashMap

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
          s"Compiled tests in $fromDir  " +
          s"[=======================================] $totalTargets/$totalTargets, " +
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
          val reporter = compile(sourceFiles, flags ++ Array("-d", dir.getAbsolutePath), false)
          completeCompilation(reporter.errorCount)
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

          // In neg-tests we allow two types of error annotations,
          // "nopos-error" which doesn't care about position and "error" which
          // has to be annotated on the correct line number.
          //
          // We collect these in a map `"file:row" -> numberOfErrors`, for
          // nopos errors we save them in `"file" -> numberOfNoPosErrors`
          val errorMap = new HashMap[String, Integer]()
          var expectedErrors = 0
          dir.listFiles.filter(_.getName.endsWith(".scala")).foreach { file =>
            Source.fromFile(file).getLines.zipWithIndex.foreach { case (line, lineNbr) =>
              val errors = line.sliding("// error".length).count(_.mkString == "// error")
              if (errors > 0)
                errorMap.put(s"${file.getAbsolutePath}:${lineNbr}", errors)

              val noposErrors = line.sliding("// nopos-error".length).count(_.mkString == "// nopos-error")
              if (noposErrors > 0)
                errorMap.put(file.getAbsolutePath, noposErrors)

              expectedErrors += noposErrors + errors
            }
          }

          val reporter = compile(sourceFiles, flags ++ Array("-d", dir.getAbsolutePath), true)
          val actualErrors = reporter.errorCount

          if (expectedErrors != actualErrors) {
            System.err.println {
              s"\nWrong number of errors encountered when compiling $dir, expected: $expectedErrors, actual: $actualErrors\n"
            }
            fail()
          }
          else if (
            // Here we check that there is a correpsonding error reported for
            // each annotation
            !reporter.errors.forall { error =>
              val fileName = error.pos.source.file.toString
              val fileAndRow = s"$fileName:${error.pos.line}"

              val rowErrors = errorMap.get(fileAndRow)
              lazy val noposErrors = errorMap.get(fileName)

              if (rowErrors ne null) {
                if (rowErrors == 1) errorMap.remove(fileAndRow)
                else errorMap.put(fileAndRow, rowErrors - 1)
                true
              }
              else if (noposErrors ne null) {
                if (noposErrors == 1) errorMap.remove(fileName)
                else errorMap.put(fileName, noposErrors - 1)
                true
              }
              else {
                System.err.println {
                  s"Error reported in ${error.pos}, but no annotation found"
                }
                false
              }
            }
          ) {
            System.err.println {
              s"\nErrors found on incorrect row numbers when compiling $dir"
            }
            fail()
          }
          else if (!errorMap.isEmpty) {
            System.err.println {
              s"\nError annotation(s) have {<error position>=<unreported error>}: $errorMap"
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

  private def compile(files: Array[JFile], flags: Array[String], suppressErrors: Boolean): DaftReporter = {

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
    reporter
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

  private def toCompilerDirFromDir(d: JFile, sourceDir: JFile, outDir: String): JFile = {
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/${d.getName}")
    // create if not exists
    targetDir.mkdirs()
    d.listFiles.foreach(copyToDir(targetDir, _))
    targetDir
  }

  private def toCompilerDirFromFile(file: JFile, sourceDir: JFile, outDir: String): JFile = {
    val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/$uniqueSubdir")
    // create if not exists
    targetDir.mkdirs()
    // copy file to dir:
    copyToDir(targetDir, file)
    targetDir
  }

  private def copyToDir(dir: JFile, file: JFile): Unit = {
    val target = Paths.get(dir.getAbsolutePath, file.getName)
    Files.copy(file.toPath, target, REPLACE_EXISTING).toFile
  }

  private def requirements(f: String, sourceDir: JFile, outDir: String): Unit = {
    require(sourceDir.isDirectory && sourceDir.exists, "passed non-directory to `compileFilesInDir`")
    require(outDir.last == '/', "please specify an `outDir` with a trailing slash")
  }

  private def compilationTargets(sourceDir: JFile): (List[JFile], List[JFile]) =
    sourceDir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
      if (f.isDirectory) (f :: dirs, files)
      else (dirs, f :: files)
    }

  def compileFileInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"
    val sourceFile = new JFile(f)
    val parent = sourceFile.getParentFile
    require(
      sourceFile.exists && !sourceFile.isDirectory &&
      (parent ne null) && parent.exists && parent.isDirectory,
      s"Source file: $f, didn't exist"
    )

    new CompilationTest(toCompilerDirFromFile(sourceFile, parent, outDir) :: Nil, f, flags)
  }

  def compileFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    requirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir)

    // Directories in which to compile all containing files with `flags`:
    val dirsToCompile =
      files.map(toCompilerDirFromFile(_, sourceDir, outDir)) ++
      dirs.map(toCompilerDirFromDir(_, sourceDir, outDir))

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(dirsToCompile, f, flags)
  }

  def compileShallowFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    requirements(f, sourceDir, outDir)

    val (_, files) = compilationTargets(sourceDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(
      files.map(toCompilerDirFromFile(_, sourceDir, outDir)),
      f,
      flags
    )
  }
}
