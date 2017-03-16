package dotty
package tools
package dotc

import java.io.{ File => JFile }
import scala.io.Source

import core.Contexts._
import reporting.{ Reporter, UniqueMessagePositions, HideNonSensicalMessages, MessageRendering }
import reporting.diagnostic.MessageContainer
import interfaces.Diagnostic.ERROR
import java.lang.reflect.InvocationTargetException
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{ Files, Path, Paths }
import java.util.concurrent.{ Executors => JExecutors, TimeUnit }
import scala.util.control.NonFatal
import java.util.HashMap

trait ParallelTesting {

  private case class Target(files: Array[JFile], outDir: JFile) {
    override def toString() = outDir.toString
  }

  private abstract class CompileRun(targets: List[Target], fromDir: String, flags: Array[String], times: Int) {
    val totalTargets = targets.length

    private[this] var _errors =  0
    def errors: Int = synchronized { _errors }

    private[this] var _targetsCompiled = 0
    private def targetsCompiled: Int = synchronized { _targetsCompiled }

    protected final def completeCompilation(newErrors: Int) = synchronized {
      _targetsCompiled += 1
      _errors += newErrors
    }

    private[this] var _failed = false
    final protected[this] def fail(): Unit = synchronized { _failed = true }
    def didFail: Boolean = _failed

    private def statusRunner: Runnable = new Runnable {
      def run(): Unit = {
        val start = System.currentTimeMillis
        var tCompiled = targetsCompiled
        while (tCompiled < totalTargets) {
          val timestamp = (System.currentTimeMillis - start) / 1000
          val progress = (tCompiled.toDouble / totalTargets * 40).toInt
          print(
            "[" + ("=" * (math.max(progress - 1, 0))) +
            (if (progress > 0) ">" else "") +
            (" " * (39 - progress)) +
            s"] compiling ($tCompiled/$totalTargets, ${timestamp}s) in '$fromDir'\r"
          )
          Thread.sleep(50)
          tCompiled = targetsCompiled
        }
        // println, otherwise no newline and cursor at start of line
        println(
          s"[=======================================] compiled ($totalTargets/$totalTargets, " +
          s"${(System.currentTimeMillis - start) / 1000}s) in '$fromDir' errors: $errors"
        )
      }
    }

    protected def compileTry(op: => Unit) =
      try op catch {
        case NonFatal(e) => {
          // if an exception is thrown during compilation, the complete test
          // run should fail
          fail()
          e.printStackTrace()
          completeCompilation(1)
          throw e
        }
      }

    protected def compilationRunnable(target: Target): Runnable

    private[ParallelTesting] def execute(): this.type = {
      assert(_targetsCompiled == 0, "not allowed to re-use a `CompileRun`")
      val pool = JExecutors.newWorkStealingPool()
      pool.submit(statusRunner)

      targets.foreach { target =>
        pool.submit(compilationRunnable(target))
      }

      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.MINUTES)
      this
    }
  }

  private final class PosCompileRun(targets: List[Target], fromDir: String, flags: Array[String], times: Int)
  extends CompileRun(targets, fromDir, flags, times) {
    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        val sourceFiles = target.files.filter(f => f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
        val reporter = compile(sourceFiles, flags, false, times, target.outDir)
        completeCompilation(reporter.errorCount)
      }
    }
  }

  private final class RunCompileRun(targets: List[Target], fromDir: String, flags: Array[String], times: Int)
  extends CompileRun(targets, fromDir, flags, times) {
    private def verifyOutput(checkFile: JFile, dir: JFile) = try {
        // Do classloading magic and running here:
        import java.net.{ URL, URLClassLoader }
        import java.io.ByteArrayOutputStream
        val ucl = new URLClassLoader(Array(dir.toURI.toURL))
        val cls = ucl.loadClass("Test")
        val meth = cls.getMethod("main", classOf[Array[String]])

        val printStream = new ByteArrayOutputStream
        Console.withOut(printStream) {
          meth.invoke(null, Array("jvm")) // partest passes at least "jvm" as an arg
        }

        val outputLines = printStream.toString("utf-8").lines.toArray
        val checkLines = Source.fromFile(checkFile).getLines.toArray

        def linesMatch =
          outputLines
          .zip(checkLines)
          .forall { case (x, y) => x == y }

        if (outputLines.length != checkLines.length || !linesMatch) {
          System.err.println {
            s"\nOutput from run test '$dir' did not match expected, output:\n${outputLines.mkString("\n")}"
          }
          fail()
        }
    }
    catch {
      case _: NoSuchMethodException =>
        System.err.println(s"\ntest in '$dir' did not contain a main method")
        fail()

      case _: ClassNotFoundException =>
        System.err.println(s"\ntest in '$dir' did was not contained within a `Test` object")
        fail()

      case _: InvocationTargetException =>
        System.err.println(s"\nTest in '$dir' might be using args(X) where X > 0")
        fail()
    }

    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        val sourceFiles = target.files.filter(f => f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
        val checkFile = target.files.find(_.getName.endsWith(".check"))
        val reporter = compile(sourceFiles, flags, false, times, target.outDir)
        completeCompilation(reporter.errorCount)

        if (reporter.errorCount == 0 && checkFile.isDefined) verifyOutput(checkFile.get, target.outDir)
        else if (reporter.errorCount > 0) {
          System.err.println(s"\nCompilation failed for: '$target'")
          fail()
        }
      }
    }
  }

  private final class NegCompileRun(targets: List[Target], fromDir: String, flags: Array[String], times: Int)
  extends CompileRun(targets, fromDir, flags, times) {
    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        val sourceFiles = target.files.filter(f => f.getName.endsWith(".scala") || f.getName.endsWith(".java"))

        // In neg-tests we allow two types of error annotations,
        // "nopos-error" which doesn't care about position and "error" which
        // has to be annotated on the correct line number.
        //
        // We collect these in a map `"file:row" -> numberOfErrors`, for
        // nopos errors we save them in `"file" -> numberOfNoPosErrors`
        val errorMap = new HashMap[String, Integer]()
        var expectedErrors = 0
        target.files.filter(_.getName.endsWith(".scala")).foreach { file =>
          Source.fromFile(file).getLines.zipWithIndex.foreach { case (line, lineNbr) =>
            val errors = line.sliding("// error".length).count(_.mkString == "// error")
            if (errors > 0)
              errorMap.put(s"${file.getAbsolutePath}:${lineNbr}", errors)

            val noposErrors = line.sliding("// nopos-error".length).count(_.mkString == "// nopos-error")
            if (noposErrors > 0) {
              val nopos = errorMap.get("nopos")
              val existing: Integer = if (nopos eq null) 0 else nopos
              errorMap.put("nopos", noposErrors + existing)
            }

            expectedErrors += noposErrors + errors
          }
        }

        val reporter = compile(sourceFiles, flags, true, times, target.outDir)
        val actualErrors = reporter.errorCount

        def missingAnnotations = !reporter.errors.forall { error =>
          val getter = if (error.pos.exists) {
            val fileName = error.pos.source.file.toString
            s"$fileName:${error.pos.line}"

          } else "nopos"

          val errors = errorMap.get(getter)

          if (errors ne null) {
            if (errors == 1) errorMap.remove(getter)
            else errorMap.put(getter, errors - 1)
            true
          }
          else {
            System.err.println {
              s"Error reported in ${error.pos.source}, but no annotation found"
            }
            false
          }
        }

        if (expectedErrors != actualErrors) {
          System.err.println {
            s"\nWrong number of errors encountered when compiling $target, expected: $expectedErrors, actual: $actualErrors\n"
          }
          fail()
        }
        else if (missingAnnotations) {
          System.err.println {
            s"\nErrors found on incorrect row numbers when compiling $target"
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
    }
  }

  private class DaftReporter(suppress: Boolean)
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages
  with MessageRendering {
    private var _errors: List[MessageContainer] = Nil
    def errors = _errors

    private var _summary = new StringBuilder
    def echoSummary(msg: String): this.type = {
      _summary.append(msg)
      this
    }

    def printSummary(): this.type = {
      val msg = _summary.toString
      if (msg.nonEmpty) println(msg)
      this
    }

    override def doReport(m: MessageContainer)(implicit ctx: Context) = {
      if (m.level == ERROR) {
        _errors = m :: _errors
        if (!suppress) System.err.println(messageAndPos(m.contained, m.pos, diagnosticLevel(m)))
      }
    }
  }

  private def compile(files0: Array[JFile], flags0: Array[String], suppressErrors: Boolean, times: Int, targetDir: JFile): DaftReporter = {

    val flags = flags0 ++ Array("-d", targetDir.getAbsolutePath)

    def flattenFiles(f: JFile): Array[JFile] =
      if (f.isDirectory) f.listFiles.flatMap(flattenFiles)
      else Array(f)

    val files: Array[JFile] = files0.flatMap(flattenFiles)

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
        s".:$scalaLib:${targetDir.getAbsolutePath}"
      ) ++ flags.takeRight(2) ++ fs

      Runtime.getRuntime.exec(fullArgs).waitFor() == 0
    } else true


    // First we try to compile the java files in the directory:
    val javaFiles = files.filter(_.getName.endsWith(".java")).map(_.getAbsolutePath)
    val javaCompiledBefore = compileWithJavac(javaFiles)

    // Then we compile the scala files:
    val reporter = new DaftReporter(suppress = suppressErrors)
    val driver =
      if (times == 1) new Driver { def newCompiler(implicit ctx: Context) = new Compiler }
      else new Driver {
        def newCompiler(implicit ctx: Context) = new Compiler

        private def ntimes(n: Int)(op: Int => Reporter): Reporter =
          (emptyReporter /: (1 to n)) ((_, i) => op(i))

        private def echoSummary(rep: Reporter, msg: String)(implicit ctx: Context) =
          rep.asInstanceOf[DaftReporter].echoSummary(msg)

        override def doCompile(comp: Compiler, files: List[String])(implicit ctx: Context) =
          ntimes(times) { run =>
            val start = System.nanoTime()
            val rep = super.doCompile(comp, files)
            echoSummary(rep, s"\ntime run $run: ${(System.nanoTime - start) / 1000000}ms")
          }
      }

    driver.process(flags ++ files.map(_.getAbsolutePath), reporter = reporter)

    // If the java files failed compilation before, we try again after:
    if (!javaCompiledBefore)
      assert(compileWithJavac(javaFiles), s"java compilation failed for ${javaFiles.mkString(", ")}")

    if (flags.contains("-verbose")) reporter.printSummary()
    else reporter
  }

  class CompilationTest private (
    targets: List[Target],
    fromDir: String,
    flags: Array[String],
    times: Int,
    shouldDelete: Boolean
  ) {
    def this(target: Target, fromDir: String, flags: Array[String]) =
      this(List(target), fromDir, flags, 1, true)

    def this(targets: List[Target], fromDir: String, flags: Array[String]) =
      this(targets, fromDir, flags, 1, true)

    def pos: this.type = {
      val run = new PosCompileRun(targets, fromDir, flags, times).execute()
      assert(run.errors == 0, s"Expected no errors when compiling $fromDir")
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
    }

    def neg: this.type = {
      assert(
        !(new NegCompileRun(targets, fromDir, flags, times).execute().didFail),
        s"Wrong number of errors encountered when compiling $fromDir"
      )
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
    }

    def run: this.type = {
      assert(
        !(new RunCompileRun(targets, fromDir, flags, times).execute().didFail),
        s"Run tests failed for test $fromDir"
      )
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
    }

    def times(i: Int): CompilationTest =
      new CompilationTest(targets, fromDir, flags, i, shouldDelete)

    def verbose: CompilationTest =
      new CompilationTest(targets, fromDir, flags ++ Array("-verbose", "-Ylog-classpath"), times, shouldDelete)

    def keepOutput: CompilationTest =
      new CompilationTest(targets, fromDir, flags, times, false)

    def delete(): Unit = targets.foreach(t => delete(t.outDir))

    def targetDirs: List[JFile] = targets.map(_.outDir)

    private def delete(file: JFile): Unit = {
      if (file.isDirectory) file.listFiles.foreach(delete)
      Files.delete(file.toPath)
    }
  }

  private def toCompilerDirFromDir(d: JFile, sourceDir: JFile, outDir: String): JFile = {
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/${d.getName}")
    targetDir.mkdirs()
    targetDir
  }

  private def toCompilerDirFromFile(file: JFile, sourceDir: JFile, outDir: String): JFile = {
    val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/$uniqueSubdir")
    targetDir.mkdirs()
    targetDir
  }

  private def copyToDir(dir: JFile, file: JFile): Unit = {
    val target = Paths.get(dir.getAbsolutePath, file.getName)
    Files.copy(file.toPath, target, REPLACE_EXISTING)
    if (file.isDirectory) file.listFiles.map(copyToDir(target.toFile, _))
  }

  private def requirements(f: String, sourceDir: JFile, outDir: String): Unit = {
    require(sourceDir.isDirectory && sourceDir.exists, "passed non-directory to `compileFilesInDir`")
    require(outDir.last == '/', "please specify an `outDir` with a trailing slash")
  }

  private def compilationTargets(sourceDir: JFile): (List[JFile], List[JFile]) =
    sourceDir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
      if (f.isDirectory) (f :: dirs, files)
      else if (f.getName.endsWith(".check")) (dirs, files)
      else if (f.getName.endsWith(".flags")) (dirs, files)
      else (dirs, f :: files)
    }

  def compileFile(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
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

    val target = Target(Array(sourceFile), toCompilerDirFromFile(sourceFile, parent, outDir))
    new CompilationTest(target, f, flags)
  }

  def compileDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    requirements(f, sourceDir, outDir)

    def flatten(f: JFile): Array[JFile] =
      if (f.isDirectory) f.listFiles.flatMap(flatten)
      else Array(f)

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir)
    targetDir.mkdirs()

    val target = Target(flatten(sourceDir), targetDir)
    new CompilationTest(target, f, flags)
  }

  def compileList(files: List[String], flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir)
    targetDir.mkdirs()
    assert(targetDir.exists, s"couldn't create target directory: $targetDir")

    val target = Target(files.map(new JFile(_)).toArray, targetDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(target, outDir.toString, flags)
  }

  def compileFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    requirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir)

    val targets =
      files.map(f => Target(Array(f), toCompilerDirFromFile(f, sourceDir, outDir))) ++
      dirs.map(dir => Target(dir.listFiles, toCompilerDirFromDir(dir, sourceDir, outDir)))

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets, f, flags)
  }

  def compileShallowFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    // each calling method gets its own unique output directory, in which we
    // place the dir being compiled:
    val callingMethod = Thread.currentThread.getStackTrace.apply(3).getMethodName
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    requirements(f, sourceDir, outDir)

    val (_, files) = compilationTargets(sourceDir)

    val targets = files.map { file =>
      Target(Array(file), toCompilerDirFromFile(file, sourceDir, outDir))
    }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets, f, flags)
  }
}
