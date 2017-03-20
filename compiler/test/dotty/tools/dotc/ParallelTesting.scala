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
import java.nio.file.{ Files, Path, Paths, NoSuchFileException }
import java.util.concurrent.{ Executors => JExecutors, TimeUnit }
import scala.util.control.NonFatal
import scala.util.Try
import scala.collection.mutable
import java.util.HashMap

trait ParallelTesting {

  def interactive: Boolean

  private sealed trait Target { self =>
    def outDir: JFile
    def flags: Array[String]

    def withFlags(newFlags: Array[String]) =
      if (!flags.containsSlice(newFlags)) self match {
        case self: ConcurrentCompilationTarget =>
          self.copy(flags = newFlags)
        case self: SeparateCompilationTarget =>
          self.copy(flags = newFlags)
      }
      else self
  }

  private final case class ConcurrentCompilationTarget(
    files: Array[JFile],
    flags: Array[String],
    outDir: JFile
  ) extends Target {
    override def toString() = outDir.toString
  }
  private final case class SeparateCompilationTarget(
    dir: JFile,
    flags: Array[String],
    outDir: JFile
  ) extends Target {

    def compilationUnits: List[Array[JFile]] =
      dir
      .listFiles
      .groupBy { file =>
        val name = file.getName
        Try {
          val potentialNumber = name
            .substring(0, name.lastIndexOf('.'))
            .reverse.takeWhile(_ != '_').reverse

          potentialNumber.toInt.toString
        }
        .toOption
        .getOrElse("")
      }
      .toList.sortBy(_._1).map(_._2.filter(isCompilable))
  }

  private abstract class CompileRun(targets: List[Target], times: Int, threadLimit: Option[Int]) {
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
            s"] compiling ($tCompiled/$totalTargets, ${timestamp}s)\r"
          )
          Thread.sleep(100)
          tCompiled = targetsCompiled
        }
        // println, otherwise no newline and cursor at start of line
        println(
          s"[=======================================] compiled ($totalTargets/$totalTargets, " +
          s"${(System.currentTimeMillis - start) / 1000}s)  "
        )
      }
    }

    protected def compileTry(op: => Unit): Unit =
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
      val pool = threadLimit match {
        case Some(i) => JExecutors.newWorkStealingPool(i)
        case None => JExecutors.newWorkStealingPool()
      }

      if (interactive) pool.submit(statusRunner)

      targets.foreach { target =>
        pool.submit(compilationRunnable(target))
      }

      pool.shutdown()
      pool.awaitTermination(10, TimeUnit.MINUTES)
      this
    }
  }

  @inline private final def isCompilable(f: JFile): Boolean = {
    val name = f.getName
    name.endsWith(".scala") || name.endsWith(".java")
  }

  private final class PosCompileRun(targets: List[Target], times: Int, threadLimit: Option[Int])
  extends CompileRun(targets, times, threadLimit) {
    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        target match {
          case ConcurrentCompilationTarget(files, flags, outDir) => {
            val sourceFiles = files.filter(isCompilable)
            val reporter = compile(sourceFiles, flags, false, times, outDir)
            completeCompilation(reporter.errorCount)
          }

          case target @ SeparateCompilationTarget(dir, flags, outDir) => {
            val compilationUnits = target.compilationUnits
            val reporters = compilationUnits.map(files => compile(files.filter(isCompilable), flags, false, times, outDir))
            completeCompilation(reporters.foldLeft(0)(_ + _.errorCount))
          }
        }

      }
    }
  }

  private final class RunCompileRun(targets: List[Target], times: Int, threadLimit: Option[Int])
  extends CompileRun(targets, times, threadLimit) {
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
        val (errorCount, hasCheckFile, doVerify) = target match {
          case ConcurrentCompilationTarget(files, flags, outDir) => {
            val sourceFiles = files.filter(isCompilable)
            val checkFile = files.find(_.getName.endsWith(".check"))
            val reporter = compile(sourceFiles, flags, false, times, outDir)

            completeCompilation(reporter.errorCount)
            (reporter.errorCount, checkFile.isDefined, () => verifyOutput(checkFile.get, outDir))
          }

          case target @ SeparateCompilationTarget(dir, flags, outDir) => {
            val checkFile = new JFile(dir.getAbsolutePath.reverse.dropWhile(_ == '/').reverse + ".check")
            val errorCount =
              target
                .compilationUnits
                .map(files => compile(files.filter(isCompilable), flags, false, times, outDir))
                .foldLeft(0)(_ + _.errorCount)

            completeCompilation(errorCount)
            (errorCount, checkFile.exists, () => verifyOutput(checkFile, outDir))
          }
        }

        if (errorCount == 0 && hasCheckFile) doVerify()
        else if (errorCount > 0) {
          System.err.println(s"\nCompilation failed for: '$target'")
          fail()
        }
      }
    }
  }

  private final class NegCompileRun(targets: List[Target], times: Int, threadLimit: Option[Int])
  extends CompileRun(targets, times, threadLimit) {
    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        // In neg-tests we allow two types of error annotations,
        // "nopos-error" which doesn't care about position and "error" which
        // has to be annotated on the correct line number.
        //
        // We collect these in a map `"file:row" -> numberOfErrors`, for
        // nopos errors we save them in `"file" -> numberOfNoPosErrors`
        def errorMapAndExpected(files: Array[JFile]): (HashMap[String, Integer], Int) = {
          val errorMap = new HashMap[String, Integer]()
          var expectedErrors = 0
          files.filter(_.getName.endsWith(".scala")).foreach { file =>
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

          (errorMap, expectedErrors)
        }

        def getMissingAnnotations(errorMap: HashMap[String, Integer], reporterErrors: List[MessageContainer]) = !reporterErrors.forall { error =>
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

        val (expectedErrors, actualErrors, hasMissingAnnotations, errorMap) = target match {
          case ConcurrentCompilationTarget(files, flags, outDir) => {
            val sourceFiles = files.filter(isCompilable)
            val (errorMap, expectedErrors) = errorMapAndExpected(sourceFiles)
            val reporter = compile(sourceFiles, flags, true, times, outDir)
            val actualErrors = reporter.errorCount

            (expectedErrors, actualErrors, () => getMissingAnnotations(errorMap, reporter.errors), errorMap)
          }

          case target @ SeparateCompilationTarget(dir, flags, outDir) => {
            val compilationUnits = target.compilationUnits
            val (errorMap, expectedErrors) = errorMapAndExpected(compilationUnits.toArray.flatten)
            val reporters = compilationUnits.map(files => compile(files.filter(isCompilable), flags, true, times, outDir))
            val actualErrors = reporters.foldLeft(0)(_ + _.errorCount)
            val errors = reporters.flatMap(_.errors)
            (expectedErrors, actualErrors, () => getMissingAnnotations(errorMap, errors), errorMap)
          }
        }

        if (expectedErrors != actualErrors) {
          System.err.println {
            s"\nWrong number of errors encountered when compiling $target, expected: $expectedErrors, actual: $actualErrors\n"
          }
          fail()
        }
        else if (hasMissingAnnotations()) {
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

    def addOutDir(xs: Array[String]): Array[String] = {
      val (beforeCp, cpAndAfter) = xs.toList.span(_ != "-classpath")
      if (cpAndAfter.nonEmpty) {
        val (cp :: cpArg :: rest) = cpAndAfter
        (beforeCp ++ (cp :: (cpArg + s":${targetDir.getAbsolutePath}") :: rest)).toArray
      }
      else (beforeCp ++ ("-classpath" :: targetDir.getAbsolutePath :: Nil)).toArray
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

    driver.process(addOutDir(flags) ++ files.map(_.getAbsolutePath), reporter = reporter)

    // If the java files failed compilation before, we try again after:
    if (!javaCompiledBefore)
      assert(compileWithJavac(javaFiles), s"java compilation failed for ${javaFiles.mkString(", ")}")

    if (flags.contains("-verbose")) reporter.printSummary()
    else reporter
  }

  final class CompilationTest private (
    private[ParallelTesting] val targets: List[Target],
    private[ParallelTesting] val times: Int,
    private[ParallelTesting] val shouldDelete: Boolean,
    private[ParallelTesting] val threadLimit: Option[Int]
  ) {
    private[ParallelTesting] def this(target: Target) =
      this(List(target), 1, true, None)

    private[ParallelTesting] def this(targets: List[Target]) =
      this(targets, 1, true, None)

    def +(other: CompilationTest) = {
      require(other.times == times, "can't combine tests that are meant to be benchmark compiled")
      require(other.shouldDelete == shouldDelete, "can't combine tests that differ on deleting output")
      new CompilationTest(targets ++ other.targets, times, shouldDelete, threadLimit)
    }

    def pos(): this.type = {
      val runErrors = new PosCompileRun(targets, times, threadLimit).execute().errors
      assert(runErrors == 0, s"Expected no errors when compiling")
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
    }

    def neg(): this.type = {
      assert(
        !(new NegCompileRun(targets, times, threadLimit).execute().didFail),
        s"Wrong number of errors encountered when compiling"
      )
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
    }

    def run(): this.type = {
      val didFail = new RunCompileRun(targets, times, threadLimit).execute().didFail
      assert(!didFail, s"Run tests failed")
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
    }

    private def copyToDir(dir: JFile, file: JFile): JFile = {
      val target = Paths.get(dir.getAbsolutePath, file.getName)
      Files.copy(file.toPath, target, REPLACE_EXISTING)
      if (file.isDirectory) file.listFiles.map(copyToDir(target.toFile, _))
      target.toFile
    }

    def copyToTarget(): CompilationTest = new CompilationTest (
      targets.map {
        case target @ ConcurrentCompilationTarget(files, _, outDir) =>
          target.copy(files = files.map(copyToDir(outDir,_)))
        case target @ SeparateCompilationTarget(dir, _, outDir) =>
          target.copy(dir = copyToDir(outDir, dir))
      },
      times, shouldDelete, threadLimit
    )

    def times(i: Int): CompilationTest =
      new CompilationTest(targets, i, shouldDelete, threadLimit)

    def verbose: CompilationTest = new CompilationTest(
      targets.map(t => t.withFlags(t.flags ++ Array("-verbose", "-Ylog-classpath"))),
      times,
      shouldDelete,
      threadLimit
    )

    def keepOutput: CompilationTest =
      new CompilationTest(targets, times, false, threadLimit)

    def limitThreads(i: Int) =
      new CompilationTest(targets, times, shouldDelete, Some(i))

    def delete(): Unit = targets.foreach(t => delete(t.outDir))

    def targetDirs: List[JFile] = targets.map(_.outDir)

    private def delete(file: JFile): Unit = {
      if (file.isDirectory) file.listFiles.foreach(delete)
      try Files.delete(file.toPath)
      catch {
        case _: NoSuchFileException => // already deleted, everything's fine
      }
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

  private def checkRequirements(f: String, sourceDir: JFile, outDir: String): Unit = {
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

  private def getCallingMethod(): String = {
    val seen = mutable.Set.empty[String]
    Thread.currentThread.getStackTrace
      .filter { elem =>
        if (seen.contains(elem.getMethodName)) false
        else { seen += elem.getMethodName; true }
      }
      .take(6).find { elem =>
        val callingClass = Class.forName(elem.getClassName)
        classOf[ParallelTesting].isAssignableFrom(callingClass) &&
        elem.getFileName != "ParallelTesting.scala"
      }
      .map(_.getMethodName)
      .getOrElse {
        throw new IllegalStateException("Unable to reflectively find calling method")
      }
  }

  def compileFile(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val sourceFile = new JFile(f)
    val parent = sourceFile.getParentFile
    val outDir =
      outDirectory + getCallingMethod + "/" +
      sourceFile.getName.substring(0, sourceFile.getName.lastIndexOf('.')) + "/"

    require(
      sourceFile.exists && !sourceFile.isDirectory &&
      (parent ne null) && parent.exists && parent.isDirectory,
      s"Source file: $f, didn't exist"
    )

    val target = ConcurrentCompilationTarget(
      Array(sourceFile),
      flags,
      toCompilerDirFromFile(sourceFile, parent, outDir)
    )
    new CompilationTest(target)
  }

  def compileDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val outDir = outDirectory + getCallingMethod + "/"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    def flatten(f: JFile): Array[JFile] =
      if (f.isDirectory) f.listFiles.flatMap(flatten)
      else Array(f)

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir + "/" + sourceDir.getName + "/")
    targetDir.mkdirs()

    val target = ConcurrentCompilationTarget(flatten(sourceDir), flags, targetDir)
    new CompilationTest(target)
  }

  def compileList(testName: String, files: List[String], flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val outDir = outDirectory + getCallingMethod + "/" + testName + "/"

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir)
    targetDir.mkdirs()
    assert(targetDir.exists, s"couldn't create target directory: $targetDir")

    val target = ConcurrentCompilationTarget(files.map(new JFile(_)).toArray, flags, targetDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(target)
  }

  def compileFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val outDir = outDirectory + getCallingMethod + "/"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir)

    val targets =
      files.map(f => ConcurrentCompilationTarget(Array(f), flags, toCompilerDirFromFile(f, sourceDir, outDir))) ++
      dirs.map(dir => SeparateCompilationTarget(dir, flags, toCompilerDirFromDir(dir, sourceDir, outDir)))

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }

  def compileShallowFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val outDir = outDirectory + getCallingMethod + "/"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (_, files) = compilationTargets(sourceDir)

    val targets = files.map { file =>
      ConcurrentCompilationTarget(Array(file), flags, toCompilerDirFromFile(file, sourceDir, outDir))
    }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }
}
