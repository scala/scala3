package dotty
package tools
package vulpix

import java.io.{ File => JFile }
import java.text.SimpleDateFormat
import java.util.HashMap
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{ Files, Path, Paths, NoSuchFileException }
import java.util.concurrent.{ Executors => JExecutors, TimeUnit, TimeoutException }

import scala.io.Source
import scala.util.control.NonFatal
import scala.util.Try
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.Random

import dotc.core.Contexts._
import dotc.reporting.{ Reporter, TestReporter }
import dotc.reporting.diagnostic.MessageContainer
import dotc.interfaces.Diagnostic.ERROR
import dotc.util.DiffUtil
import dotc.{ Driver, Compiler }

/** A parallel testing suite whose goal is to integrate nicely with JUnit
 *
 *  This trait can be mixed in to offer parallel testing to compile runs. When
 *  using this, you should be running your JUnit tests **sequentially**, as the
 *  test suite itself runs with a high level of concurrency.
 */
trait ParallelTesting extends RunnerOrchestration { self =>

  import ParallelTesting._

  /** If the running environment supports an interactive terminal, each `Test`
   *  will be run with a progress bar and real time feedback
   */
  def isInteractive: Boolean

  /** A string which is used to filter which tests to run, if `None` will run
   *  all tests. All absolute paths that contain the substring `testFilter`
   *  will be run
   */
  def testFilter: Option[String]

  /** A test source whose files or directory of files is to be compiled
   *  in a specific way defined by the `Test`
   */
  private sealed trait TestSource { self =>
    def name: String
    def outDir: JFile
    def flags: Array[String]

    def classPath: String =
      outDir.getAbsolutePath +
      flags
        .dropWhile(_ != "-classpath")
        .drop(1)
        .headOption
        .map(":" + _)
        .getOrElse("")

    def runClassPath = {
      flags
        .dropWhile(_ != "-YRunClasspath")
        .drop(1)
        .headOption
        .map(outDir.getAbsolutePath + ":" + _)
        .getOrElse(classPath)
    }

    def title: String = self match {
      case self: JointCompilationSource =>
        if (self.files.length > 1) name
        else self.files.head.getPath

      case self: SeparateCompilationSource =>
        self.dir.getPath
    }

    /** Adds the flags specified in `newFlags0` if they do not already exist */
    def withFlags(newFlags0: String*) = {
      val newFlags = newFlags0.toArray
      if (!flags.containsSlice(newFlags)) self match {
        case self: JointCompilationSource =>
          self.copy(flags = flags ++ newFlags)
        case self: SeparateCompilationSource =>
          self.copy(flags = flags ++ newFlags)
      }
      else self
    }

    /** Generate the instructions to redo the test from the command line */
    def buildInstructions(errors: Int, warnings: Int): String = {
      val sb = new StringBuilder
      val maxLen = 80
      var lineLen = 0

      sb.append(
        s"""|
            |Test '$title' compiled with $errors error(s) and $warnings warning(s),
            |the test can be reproduced by running:""".stripMargin
      )
      sb.append("\n\n./bin/dotc ")
      flags.foreach { arg =>
        if (lineLen > maxLen) {
          sb.append(" \\\n        ")
          lineLen = 4
        }
        sb.append(arg)
        lineLen += arg.length
        sb += ' '
      }

      self match {
        case JointCompilationSource(_, files, _, _) => {
          files.map(_.getAbsolutePath).foreach { path =>
            sb.append("\\\n        ")
            sb.append(path)
            sb += ' '
          }
          sb.toString + "\n\n"
        }
        case self: SeparateCompilationSource => {
          val command = sb.toString
          val fsb = new StringBuilder(command)
          self.compilationGroups.foreach { files =>
            files.map(_.getPath).foreach { path =>
              fsb.append("\\\n        ")
              lineLen = 8
              fsb.append(path)
              fsb += ' '
            }
            fsb.append("\n\n")
            fsb.append(command)
          }
          fsb.toString + "\n\n"
        }
      }
    }
  }

  /** A group of files that may all be compiled together, with the same flags
   *  and output directory
   */
  private final case class JointCompilationSource(
    name: String,
    files: Array[JFile],
    flags: Array[String],
    outDir: JFile
  ) extends TestSource {
    def sourceFiles: Array[JFile] = files.filter(isSourceFile)

    override def toString() = outDir.toString
  }

  /** A test source whose files will be compiled separately according to their
   *  suffix `_X`
   */
  private final case class SeparateCompilationSource(
    name: String,
    dir: JFile,
    flags: Array[String],
    outDir: JFile
  ) extends TestSource {

    /** Get the files grouped by `_X` as a list of groups, files missing this
     *  suffix will be put into the same group
     *
     *  Filters out all none source files
     */
    def compilationGroups: List[Array[JFile]] =
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
      .toList.sortBy(_._1).map(_._2.filter(isSourceFile))
  }

  /** Each `Test` takes the `testSources` and performs the compilation and assertions
   *  according to the implementing class "neg", "run" or "pos".
   */
  private abstract class Test(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit val summaryReport: SummaryReporting) { test =>

    import summaryReport._

    protected final val realStdout = System.out
    protected final val realStderr = System.err

    /** A runnable that logs its contents in a buffer */
    trait LoggedRunnable extends Runnable {
      /** Instances of `LoggedRunnable` implement this method instead of the
       *  `run` method
       */
      def checkTestSource(): Unit

      private[this] val logBuffer = mutable.ArrayBuffer.empty[String]
      def log(msg: String): Unit = logBuffer.append(msg)

      def logReporterContents(reporter: TestReporter): Unit =
        reporter.messages.foreach(log)

      def echo(msg: String): Unit = {
        log(msg)
        test.echo(msg)
      }

      final def run(): Unit = {
        checkTestSource()
        summaryReport.echoToLog(logBuffer.iterator)
      }
    }

    /** Actual compilation run logic, the test behaviour is defined here */
    protected def encapsulatedCompilation(testSource: TestSource): LoggedRunnable

    /** All testSources left after filtering out */
    private val filteredSources =
      if (!testFilter.isDefined) testSources
      else testSources.filter {
        case JointCompilationSource(_, files, _, _) =>
          files.exists(file => file.getAbsolutePath.contains(testFilter.get))
        case SeparateCompilationSource(_, dir, _, _) =>
          dir.getAbsolutePath.contains(testFilter.get)
      }

    /** Total amount of test sources being compiled by this test */
    val sourceCount = filteredSources.length

    private[this] var _errorCount =  0
    def errorCount: Int = _errorCount

    private[this] var _testSourcesCompleted = 0
    private def testSourcesCompleted: Int = _testSourcesCompleted

    /** Complete the current compilation with the amount of errors encountered */
    protected final def registerCompletion(errors: Int) = synchronized {
      _testSourcesCompleted += 1
      _errorCount += errors
    }

    sealed trait Failure
    case class JavaCompilationFailure(reason: String) extends Failure
    case class TimeoutFailure(title: String) extends Failure
    case object Generic extends Failure

    private[this] var _failures = Set.empty[Failure]
    /** Fail the current test */
    protected[this] final def fail(failure: Failure = Generic): Unit = synchronized {
      _failures = _failures + failure
    }
    def didFail: Boolean = _failures.nonEmpty

    /** A set of the different failures */
    def failureReasons: Set[Failure] = _failures

    protected def logBuildInstructions(reporter: TestReporter, testSource: TestSource, err: Int, war: Int) = {
      val errorMsg = testSource.buildInstructions(reporter.errorCount, reporter.warningCount)
      addFailureInstruction(errorMsg)
      failTestSource(testSource)
    }

    /** Instructions on how to reproduce failed test source compilations */
    private[this] val reproduceInstructions = mutable.ArrayBuffer.empty[String]
    protected final def addFailureInstruction(ins: String): Unit =
      synchronized { reproduceInstructions.append(ins) }

    /** The test sources that failed according to the implementing subclass */
    private[this] val failedTestSources = mutable.ArrayBuffer.empty[String]
    protected final def failTestSource(testSource: TestSource, reason: Failure = Generic) = synchronized {
      val extra = reason match {
        case TimeoutFailure(title) => s", test '$title' timed out"
        case JavaCompilationFailure(msg) => s", java test sources failed to compile with: \n$msg"
        case Generic => ""
      }
      failedTestSources.append(testSource.title + s" failed" + extra)
      fail(reason)
    }

    /** Prints to `System.err` if we're not suppressing all output */
    protected def echo(msg: String): Unit = if (!suppressAllOutput) {
      // pad right so that output is at least as large as progress bar line
      val paddingRight = " " * math.max(0, 80 - msg.length)
      realStderr.println(msg + paddingRight)
    }

    /** A single `Runnable` that prints a progress bar for the curent `Test` */
    private def createProgressMonitor: Runnable = new Runnable {
      def run(): Unit = {
        val start = System.currentTimeMillis
        var tCompiled = testSourcesCompleted
        while (tCompiled < sourceCount) {
          val timestamp = (System.currentTimeMillis - start) / 1000
          val progress = (tCompiled.toDouble / sourceCount * 40).toInt

          realStdout.print(
            "[" + ("=" * (math.max(progress - 1, 0))) +
            (if (progress > 0) ">" else "") +
            (" " * (39 - progress)) +
            s"] completed ($tCompiled/$sourceCount, ${timestamp}s)\r"
          )

          Thread.sleep(100)
          tCompiled = testSourcesCompleted
        }
        // println, otherwise no newline and cursor at start of line
        realStdout.println(
          s"[=======================================] completed ($sourceCount/$sourceCount, " +
          s"${(System.currentTimeMillis - start) / 1000}s)  "
        )
      }
    }

    /** Wrapper function to make sure that the compiler itself did not crash -
     *  if it did, the test should automatically fail.
     */
    protected def tryCompile(testSource: TestSource)(op: => Unit): Unit =
      try {
        val testing = s"Testing ${testSource.title}"
        summaryReport.echoToLog(testing)
        if (!isInteractive) realStdout.println(testing)
        op
      } catch {
        case e: Throwable => {
          // if an exception is thrown during compilation, the complete test
          // run should fail
          failTestSource(testSource)
          e.printStackTrace()
          registerCompletion(1)
          throw e
        }
      }

    protected def compile(files0: Array[JFile], flags0: Array[String], suppressErrors: Boolean, targetDir: JFile): TestReporter = {

      val flags = flags0 ++ Array("-d", targetDir.getAbsolutePath)

      def flattenFiles(f: JFile): Array[JFile] =
        if (f.isDirectory) f.listFiles.flatMap(flattenFiles)
        else Array(f)

      val files: Array[JFile] = files0.flatMap(flattenFiles)

      def addOutDir(xs: Array[String]): Array[String] = {
        val (beforeCp, cpAndAfter) = xs.toList.span(_ != "-classpath")
        if (cpAndAfter.nonEmpty) {
          val (cp :: cpArg :: rest) = cpAndAfter
          (beforeCp ++ (cp :: (cpArg + s":${targetDir.getAbsolutePath}") :: rest)).toArray
        }
        else (beforeCp ++ ("-classpath" :: targetDir.getAbsolutePath :: Nil)).toArray
      }

      def compileWithJavac(fs: Array[String]) = if (fs.nonEmpty) {
        val fullArgs = Array(
          "javac",
          "-encoding", "UTF-8",
          "-classpath",
          s".:${Jars.scalaLibrary}:${targetDir.getAbsolutePath}"
        ) ++ flags.takeRight(2) ++ fs

        val process = Runtime.getRuntime.exec(fullArgs)
        val output = Source.fromInputStream(process.getErrorStream).mkString

        if (process.waitFor() != 0) Some(output)
        else None
      } else None

      val reporter =
        TestReporter.reporter(realStdout, logLevel =
          if (suppressErrors || suppressAllOutput) ERROR + 1 else ERROR)

      val driver =
        if (times == 1) new Driver
        else new Driver {
          private def ntimes(n: Int)(op: Int => Reporter): Reporter =
            (emptyReporter /: (1 to n)) ((_, i) => op(i))

          override def doCompile(comp: Compiler, files: List[String])(implicit ctx: Context) =
            ntimes(times) { run =>
              val start = System.nanoTime()
              val rep = super.doCompile(comp, files)
              ctx.echo(s"\ntime run $run: ${(System.nanoTime - start) / 1000000}ms")
              rep
            }
        }

      val allArgs = addOutDir(flags)

      // Compile with a try to catch any StackTrace generated by the compiler:
      try {
        driver.process(allArgs ++ files.map(_.getAbsolutePath), reporter = reporter)

        val javaFiles = files.filter(_.getName.endsWith(".java")).map(_.getAbsolutePath)
        val javaErrors = compileWithJavac(javaFiles)

        if (javaErrors.isDefined) {
          echo(s"\njava compilation failed: \n${ javaErrors.get }")
          fail(failure = JavaCompilationFailure(javaErrors.get))
        }
      }
      catch {
        case NonFatal(ex) => reporter.logStackTrace(ex)
      }

      reporter
    }

    private[ParallelTesting] def executeTestSuite(): this.type = {
      assert(_testSourcesCompleted == 0, "not allowed to re-use a `CompileRun`")

      if (filteredSources.nonEmpty) {
        val pool = threadLimit match {
          case Some(i) => JExecutors.newWorkStealingPool(i)
          case None => JExecutors.newWorkStealingPool()
        }

        if (isInteractive && !suppressAllOutput) pool.submit(createProgressMonitor)

        filteredSources.foreach { target =>
          pool.submit(encapsulatedCompilation(target))
        }

        pool.shutdown()
        if (!pool.awaitTermination(20, TimeUnit.MINUTES)) {
          pool.shutdownNow()
          System.setOut(realStdout)
          System.setErr(realStderr)
          throw new TimeoutException("Compiling targets timed out")
        }

        if (didFail) {
          reportFailed()
          failedTestSources.toSet.foreach(addFailedTest)
          reproduceInstructions.iterator.foreach(addReproduceInstruction)
        }
        else reportPassed()
      }
      else echo {
        testFilter
          .map(r => s"""No files matched "$r" in test""")
          .getOrElse("No tests available under target - erroneous test?")
      }

      this
    }
  }

  private final class PosTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    protected def encapsulatedCompilation(testSource: TestSource) = new LoggedRunnable {
      def checkTestSource(): Unit = tryCompile(testSource) {
        testSource match {
          case testSource @ JointCompilationSource(_, files, flags, outDir) => {
            val reporter = compile(testSource.sourceFiles, flags, false, outDir)
            registerCompletion(reporter.errorCount)

            if (reporter.compilerCrashed || reporter.errorCount > 0) {
              logReporterContents(reporter)
              logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)
            }
          }

          case testSource @ SeparateCompilationSource(_, dir, flags, outDir) => {
            val reporters = testSource.compilationGroups.map(files => compile(files, flags, false, outDir))
            val compilerCrashed = reporters.exists(_.compilerCrashed)
            val errorCount = reporters.foldLeft(0) { (acc, reporter) =>
              if (reporter.errorCount > 0)
                logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)

              acc + reporter.errorCount
            }

            def warningCount = reporters.foldLeft(0)(_ + _.warningCount)

            registerCompletion(errorCount)

            if (compilerCrashed || errorCount > 0) {
              reporters.foreach(logReporterContents)
              logBuildInstructions(reporters.head, testSource, errorCount, warningCount)
            }
          }
        }
      }
    }
  }

  private final class RunTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    private[this] var didAddNoRunWarning = false
    private[this] def addNoRunWarning() = if (!didAddNoRunWarning) {
      didAddNoRunWarning = true
      summaryReport.addStartingMessage {
        """|WARNING
           |-------
           |Run tests were only compiled, not run - this is due to the `dotty.tests.norun`
           |property being set
           |""".stripMargin
      }
    }

    private def verifyOutput(checkFile: Option[JFile], dir: JFile, testSource: TestSource, warnings: Int) = {
      if (Properties.testsNoRun) addNoRunWarning()
      else runMain(testSource.runClassPath) match {
        case Success(_) if !checkFile.isDefined || !checkFile.get.exists => // success!
        case Success(output) => {
          val outputLines = output.lines.toArray
          val checkLines: Array[String] = Source.fromFile(checkFile.get).getLines().toArray
          val sourceTitle = testSource.title

          def linesMatch =
            outputLines
            .zip(checkLines)
            .forall { case (x, y) => x == y }

          if (outputLines.length != checkLines.length || !linesMatch) {
            // Print diff to files and summary:
            val diff = outputLines.zip(checkLines).map { case (act, exp) =>
              DiffUtil.mkColoredLineDiff(exp, act)
            }.mkString("\n")

            val msg =
              s"""|Output from '$sourceTitle' did not match check file.
                  |Diff ('e' is expected, 'a' is actual):
                  |""".stripMargin + diff + "\n"
            echo(msg)
            addFailureInstruction(msg)

            // Print build instructions to file and summary:
            val buildInstr = testSource.buildInstructions(0, warnings)
            addFailureInstruction(buildInstr)

            // Fail target:
            failTestSource(testSource)
          }
        }

        case Failure(output) =>
          echo(s"Test '${testSource.title}' failed with output:")
          echo(output)
          failTestSource(testSource)

        case Timeout =>
          echo("failed because test " + testSource.title + " timed out")
          failTestSource(testSource, TimeoutFailure(testSource.title))
      }
    }

    protected def encapsulatedCompilation(testSource: TestSource) = new LoggedRunnable {
      def checkTestSource(): Unit = tryCompile(testSource) {
        val (compilerCrashed, errorCount, warningCount, verifier: Function0[Unit]) = testSource match {
          case testSource @ JointCompilationSource(_, files, flags, outDir) => {
            val checkFile = files.flatMap { file =>
              if (file.isDirectory) Nil
              else {
                val fname = file.getAbsolutePath.reverse.dropWhile(_ != '.').reverse + "check"
                val checkFile = new JFile(fname)
                if (checkFile.exists) List(checkFile)
                else Nil
              }
            }.headOption
            val reporter = compile(testSource.sourceFiles, flags, false, outDir)

            if (reporter.compilerCrashed || reporter.errorCount > 0) {
              logReporterContents(reporter)
              logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)
            }

            (reporter.compilerCrashed, reporter.errorCount, reporter.warningCount, () => verifyOutput(checkFile, outDir, testSource, reporter.warningCount))
          }

          case testSource @ SeparateCompilationSource(_, dir, flags, outDir) => {
            val checkFile = new JFile(dir.getAbsolutePath.reverse.dropWhile(_ == '/').reverse + ".check")
            val reporters = testSource.compilationGroups.map(compile(_, flags, false, outDir))
            val compilerCrashed = reporters.exists(_.compilerCrashed)
            val (errorCount, warningCount) =
              reporters.foldLeft((0,0)) { case ((errors, warnings), reporter) =>
                if (reporter.errorCount > 0)
                  logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)

                (errors + reporter.errorCount, warnings + reporter.warningCount)
              }

            if (errorCount > 0) {
              reporters.foreach(logReporterContents)
              logBuildInstructions(reporters.head, testSource, errorCount, warningCount)
            }

            (compilerCrashed, errorCount, warningCount, () => verifyOutput(Some(checkFile), outDir, testSource, warningCount))
          }
        }

        if (!compilerCrashed && errorCount == 0) verifier()
        else {
          echo(s"    Compilation failed for: '${testSource.title}'                               ")
          val buildInstr = testSource.buildInstructions(errorCount, warningCount)
          addFailureInstruction(buildInstr)
          failTestSource(testSource)
        }
        registerCompletion(errorCount)
      }
    }
  }

  private final class NegTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    protected def encapsulatedCompilation(testSource: TestSource) = new LoggedRunnable {
      def checkTestSource(): Unit = tryCompile(testSource) {
        // In neg-tests we allow two types of error annotations,
        // "nopos-error" which doesn't care about position and "error" which
        // has to be annotated on the correct line number.
        //
        // We collect these in a map `"file:row" -> numberOfErrors`, for
        // nopos errors we save them in `"file" -> numberOfNoPosErrors`
        def getErrorMapAndExpectedCount(files: Array[JFile]): (HashMap[String, Integer], Int) = {
          val errorMap = new HashMap[String, Integer]()
          var expectedErrors = 0
          files.filter(_.getName.endsWith(".scala")).foreach { file =>
            Source.fromFile(file).getLines().zipWithIndex.foreach { case (line, lineNbr) =>
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

        def getMissingExpectedErrors(errorMap: HashMap[String, Integer], reporterErrors: Iterator[MessageContainer]) = !reporterErrors.forall { error =>
          val key = if (error.pos.exists) {
            val fileName = error.pos.source.file.toString
            s"$fileName:${error.pos.line}"

          } else "nopos"

          val errors = errorMap.get(key)

          if (errors ne null) {
            if (errors == 1) errorMap.remove(key)
            else errorMap.put(key, errors - 1)
            true
          }
          else {
            echo {
              s"Error reported in ${error.pos.source}, but no annotation found"
            }
            false
          }
        }

        val (compilerCrashed, expectedErrors, actualErrors, hasMissingAnnotations, errorMap) = testSource match {
          case testSource @ JointCompilationSource(_, files, flags, outDir) => {
            val sourceFiles = testSource.sourceFiles
            val (errorMap, expectedErrors) = getErrorMapAndExpectedCount(sourceFiles)
            val reporter = compile(sourceFiles, flags, true, outDir)
            val actualErrors = reporter.errorCount

            if (reporter.compilerCrashed || actualErrors > 0)
              logReporterContents(reporter)

            (reporter.compilerCrashed, expectedErrors, actualErrors, () => getMissingExpectedErrors(errorMap, reporter.errors), errorMap)
          }

          case testSource @ SeparateCompilationSource(_, dir, flags, outDir) => {
            val compilationGroups = testSource.compilationGroups
            val (errorMap, expectedErrors) = getErrorMapAndExpectedCount(compilationGroups.toArray.flatten)
            val reporters = compilationGroups.map(compile(_, flags, true, outDir))
            val compilerCrashed = reporters.exists(_.compilerCrashed)
            val actualErrors = reporters.foldLeft(0)(_ + _.errorCount)
            val errors = reporters.iterator.flatMap(_.errors)

            if (actualErrors > 0)
              reporters.foreach(logReporterContents)

            (compilerCrashed, expectedErrors, actualErrors, () => getMissingExpectedErrors(errorMap, errors), errorMap)
          }
        }

        if (compilerCrashed) {
          echo(s"Compiler crashed when compiling: ${testSource.title}")
          failTestSource(testSource)
        }
        else if (expectedErrors != actualErrors) {
          echo {
            s"\nWrong number of errors encountered when compiling $testSource, expected: $expectedErrors, actual: $actualErrors\n"
          }
          failTestSource(testSource)
        }
        else if (hasMissingAnnotations()) {
          echo {
            s"\nErrors found on incorrect row numbers when compiling $testSource"
          }
          failTestSource(testSource)
        }
        else if (!errorMap.isEmpty) {
          echo {
            s"\nExpected error(s) have {<error position>=<unreported error>}: $errorMap"
          }
          failTestSource(testSource)
        }

        registerCompletion(actualErrors)
      }
    }
  }

  /** The `CompilationTest` is the main interface to `ParallelTesting`, it
   *  can be instantiated via one of the following methods:
   *
   *  - `compileFile`
   *  - `compileDir`
   *  - `compileList`
   *  - `compileFilesInDir`
   *  - `compileShallowFilesInDir`
   *
   *  Each compilation test can then be turned into either a "pos", "neg" or
   *  "run" test:
   *
   *  ```
   *  compileFile("../tests/pos/i1103.scala", opts).pos()
   *  ```
   *
   *  These tests can be customized before calling one of the execution
   *  methods, for instance:
   *
   *  ```
   *  compileFile("../tests/pos/i1103.scala", opts).times(2).verbose.pos()
   *  ```
   *
   *  Which would compile `i1103.scala` twice with the verbose flag as a "pos"
   *  test.
   *
   *  pos tests
   *  =========
   *  Pos tests verify that the compiler is able to compile the given
   *  `TestSource`s and that they generate no errors or exceptions during
   *  compilation
   *
   *  neg tests
   *  =========
   *  Neg tests are expected to generate a certain amount of errors - but not
   *  crash the compiler. In each `.scala` file, you specifiy the line on which
   *  the error will be generated, e.g:
   *
   *  ```
   *  val x: String = 1 // error
   *  ```
   *
   *  if a line generates multiple errors, you need to annotate it multiple
   *  times. For a line that generates two errors:
   *
   *  ```
   *  val y: String = { val y1: String = 1; 2 } // error // error
   *  ```
   *
   *  Certain errors have no position, if you need to check these annotate the
   *  file anywhere with `// nopos-error`
   *
   *  run tests
   *  =========
   *  Run tests are a superset of pos tests, they both verify compilation and
   *  that the compiler does not crash. In addition, run tests verify that the
   *  tests are able to run as expected.
   *
   *  Run tests need to have the following form:
   *
   *  ```
   *  object Test {
   *    def main(args: Array[String]): Unit = ()
   *  }
   *  ```
   *
   *  This is because the runner instantiates the `Test` class and calls the
   *  main method.
   *
   *  Other definitions are allowed in the same file, but the file needs to at
   *  least have the `Test` object with a `main` method.
   *
   *  To verify output you may use `.check` files. These files should share the
   *  name of the file or directory that they are testing. For instance:
   *
   *  ```none
   *  .
   *  └── tests
   *      ├── i1513.scala
   *      └── i1513.check
   *  ```
   *
   *  If you are testing a directory under separate compilation, you would
   *  have:
   *
   *  ```none
   *  .
   *  └── tests
   *      ├── myTestDir
   *      │   ├── T_1.scala
   *      │   ├── T_2.scala
   *      │   └── T_3.scala
   *      └── myTestDir.check
   *  ```
   *
   *  In the above example, `i1513.scala` and one of the files `T_X.scala`
   *  would contain a `Test` object with a main method.
   *
   *  Composing tests
   *  ===============
   *  Since this is a parallel test suite, it is essential to be able to
   *  compose tests to take advantage of the concurrency. This is done using
   *  the `+` function. This function will make sure that tests being combined
   *  are compatible according to the `require`s in `+`.
   */
  final class CompilationTest private (
    private[ParallelTesting] val targets: List[TestSource],
    private[ParallelTesting] val times: Int,
    private[ParallelTesting] val shouldDelete: Boolean,
    private[ParallelTesting] val threadLimit: Option[Int],
    private[ParallelTesting] val shouldFail: Boolean,
    private[ParallelTesting] val shouldSuppressOutput: Boolean
  ) {
    import org.junit.Assert.fail

    private[ParallelTesting] def this(target: TestSource) =
      this(List(target), 1, true, None, false, false)

    private[ParallelTesting] def this(targets: List[TestSource]) =
      this(targets, 1, true, None, false, false)

    /** Compose test targets from `this` with `other`
     *
     *  It does this, only if the two tests are compatible. Otherwise it throws
     *  an `IllegalArgumentException`.
     *
     *  Grouping tests together like this allows us to take advantage of the
     *  concurrency offered by this test suite as each call to an executing
     *  method (`pos()` / `checkExpectedErrors()`/ `run()`) will spin up a thread pool with the
     *  maximum allowed level of concurrency. Doing this for only a few targets
     *  does not yield any real benefit over sequential compilation.
     *
     *  As such, each `CompilationTest` should contain as many targets as
     *  possible.
     */
    def +(other: CompilationTest) = {
      require(other.times == times, "can't combine tests that are meant to be benchmark compiled")
      require(other.shouldDelete == shouldDelete, "can't combine tests that differ on deleting output")
      require(other.shouldFail == shouldFail, "can't combine tests that have different expectations on outcome")
      require(other.shouldSuppressOutput == shouldSuppressOutput, "can't combine tests that both suppress and don't suppress output")
      new CompilationTest(targets ++ other.targets, times, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput)
    }

    /** Creates a "pos" test run, which makes sure that all tests pass
     *  compilation without generating errors and that they do not crash the
     *  compiler
     */
    def checkCompile()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new PosTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput).executeTestSuite()

      if (!shouldFail && test.didFail) {
        fail(s"Expected no errors when compiling, failed for the following reason(s):\n${ reasonsForFailure(test) }")
      }
      else if (shouldFail && !test.didFail) {
        fail("Pos test should have failed, but didn't")
      }

      cleanup()
    }

    /** Creates a "neg" test run, which makes sure that each test generates the
     *  correct amount of errors at the correct positions. It also makes sure
     *  that none of these tests crash the compiler
     */
    def checkExpectedErrors()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new NegTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput).executeTestSuite()

      if (!shouldFail && test.didFail) {
        fail(s"Neg test shouldn't have failed, but did. Reasons:\n${ reasonsForFailure(test) }")
      }
      else if (shouldFail && !test.didFail) {
        fail("Neg test should have failed, but did not")
      }

      cleanup()
    }

    /** Creates a "run" test run, which is a superset of "pos". In addition to
     *  making sure that all tests pass compilation and that they do not crash
     *  the compiler; it also makes sure that all tests can run with the
     *  expected output
     */
    def checkRuns()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new RunTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput).executeTestSuite()

      if (!shouldFail && test.didFail) {
        fail(s"Run test failed, but should not, reasons:\n${ reasonsForFailure(test) }")
      }
      else if (shouldFail && !test.didFail) {
        fail("Run test should have failed, but did not")
      }

      cleanup()
    }

    /** Deletes output directories and files */
    private def cleanup(): this.type = {
      if (shouldDelete) delete()
      this
    }

    /** Extract `Failure` set and render from `Test` */
    private[this] def reasonsForFailure(test: Test): String = {
      val errors =
        if (test.errorCount == 0) ""
        else s"\n  - encountered ${test.errorCount} error(s)"

      errors + test.failureReasons.collect {
        case test.TimeoutFailure(title) =>
          s"  - test '$title' timed out"
        case test.JavaCompilationFailure(msg) =>
          s"  - java compilation failed with:\n${ msg.lines.map("      " + _).mkString("\n") }"
      }.mkString("\n")
    }

    /** Copies `file` to `dir` - taking into account if `file` is a directory,
     *  and if so copying recursively
     */
    private def copyToDir(dir: JFile, file: JFile): JFile = {
      val target = Paths.get(dir.getAbsolutePath, file.getName)
      Files.copy(file.toPath, target, REPLACE_EXISTING)
      if (file.isDirectory) file.listFiles.map(copyToDir(target.toFile, _))
      target.toFile
    }

    /** Builds a new `CompilationTest` where we have copied the target files to
     *  the out directory. This is needed for tests that modify the original
     *  source, such as `-rewrite` tests
     */
    def copyToTarget(): CompilationTest = new CompilationTest (
      targets.map {
        case target @ JointCompilationSource(_, files, _, outDir) =>
          target.copy(files = files.map(copyToDir(outDir,_)))
        case target @ SeparateCompilationSource(_, dir, _, outDir) =>
          target.copy(dir = copyToDir(outDir, dir))
      },
      times, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput
    )

    /** Builds a `CompilationTest` which performs the compilation `i` times on
     *  each target
     */
    def times(i: Int): CompilationTest =
      new CompilationTest(targets, i, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput)

    /** Builds a `Compilationtest` which passes the verbose flag and logs the
     *  classpath
     */
    def verbose: CompilationTest = new CompilationTest(
      targets.map(t => t.withFlags("-verbose", "-Ylog-classpath")),
      times, shouldDelete, threadLimit, shouldFail, shouldSuppressOutput
    )

    /** Builds a `CompilationTest` which keeps the generated output files
     *
     *  This is needed for tests like `tastyBootstrap` which relies on first
     *  compiling a certain part of the project and then compiling a second
     *  part which depends on the first
     */
    def keepOutput: CompilationTest =
      new CompilationTest(targets, times, false, threadLimit, shouldFail, shouldSuppressOutput)

    /** Builds a `CompilationTest` with a limited level of concurrency with
     *  maximum `i` threads
     */
    def limitThreads(i: Int): CompilationTest =
      new CompilationTest(targets, times, shouldDelete, Some(i), shouldFail, shouldSuppressOutput)

    /** Builds a `CompilationTest` where the executed test is expected to fail
     *
     *  This behaviour is mainly needed for the tests that test the test suite.
     */
    def expectFailure: CompilationTest =
      new CompilationTest(targets, times, shouldDelete, threadLimit, true, shouldSuppressOutput)

    /** Builds a `CompilationTest` where all output is suppressed
     *
     *  This behaviour is mainly needed for the tests that test the test suite.
     */
    def suppressAllOutput: CompilationTest =
      new CompilationTest(targets, times, shouldDelete, threadLimit, shouldFail, true)

    /** Delete all output files generated by this `CompilationTest` */
    def delete(): Unit = targets.foreach(t => delete(t.outDir))

    private def delete(file: JFile): Unit = {
      if (file.isDirectory) file.listFiles.foreach(delete)
      try Files.delete(file.toPath)
      catch {
        case _: NoSuchFileException => // already deleted, everything's fine
      }
    }
  }

  /** Create out directory for directory `d` */
  private def createOutputDirsForDir(d: JFile, sourceDir: JFile, outDir: String): JFile = {
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/${d.getName}")
    targetDir.mkdirs()
    targetDir
  }

  /** Create out directory for `file` */
  private def createOutputDirsForFile(file: JFile, sourceDir: JFile, outDir: String): JFile = {
    val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/$uniqueSubdir")
    targetDir.mkdirs()
    targetDir
  }

  /** Make sure that directory string is as expected */
  private def checkRequirements(f: String, sourceDir: JFile, outDir: String): Unit = {
    require(sourceDir.isDirectory && sourceDir.exists, "passed non-directory to `compileFilesInDir`")
    require(outDir.last == '/', "please specify an `outDir` with a trailing slash")
  }

  /** Separates directories from files and returns them as `(dirs, files)` */
  private def compilationTargets(sourceDir: JFile): (List[JFile], List[JFile]) =
    sourceDir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
      if (f.isDirectory) (f :: dirs, files)
      else if (isSourceFile(f)) (dirs, f :: files)
      else (dirs, files)
    }

  /** Gets the name of the calling method via reflection.
   *
   *  It does this in a way that needs to work both with the bootstrapped dotty
   *  and the non-bootstrapped version. Since the two compilers generate
   *  different bridges, we first need to filter out methods with the same name
   *  (bridges) - and then find the `@Test` method in our extending class
   */
  private def getCallingMethod(): String = {
    val seen = mutable.Set.empty[String]
    Thread.currentThread.getStackTrace
      .filter { elem =>
        if (seen.contains(elem.getMethodName)) false
        else { seen += elem.getMethodName; true }
      }
      .find { elem =>
        val callingClass = Class.forName(elem.getClassName)
        classOf[ParallelTesting].isAssignableFrom(callingClass) &&
        elem.getFileName != "ParallelTesting.scala"
      }
      .map(_.getMethodName)
      .getOrElse {
        throw new IllegalStateException("Unable to reflectively find calling method")
      }
      .takeWhile(_ != '$')
  }

  /** Compiles a single file from the string path `f` using the supplied flags */
  def compileFile(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val callingMethod = getCallingMethod()
    val sourceFile = new JFile(f)
    val parent = sourceFile.getParentFile
    val outDir =
      outDirectory + callingMethod + "/" +
      sourceFile.getName.substring(0, sourceFile.getName.lastIndexOf('.')) + "/"

    require(
      sourceFile.exists && !sourceFile.isDirectory &&
      (parent ne null) && parent.exists && parent.isDirectory,
      s"Source file: $f, didn't exist"
    )

    val target = JointCompilationSource(
      callingMethod,
      Array(sourceFile),
      flags,
      createOutputDirsForFile(sourceFile, parent, outDir)
    )
    new CompilationTest(target)
  }

  /** Compiles a directory `f` using the supplied `flags`. This method does
   *  deep compilation, that is - it compiles all files and subdirectories
   *  contained within the directory `f`.
   *
   *  By default, files are compiled in alphabetical order. An optional seed
   *  can be used for randomization.
   */
  def compileDir(f: String, flags: Array[String], randomOrder: Option[Int] = None)(implicit outDirectory: String): CompilationTest = {
    val callingMethod = getCallingMethod()
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    def flatten(f: JFile): Array[JFile] =
      if (f.isDirectory) f.listFiles.flatMap(flatten)
      else Array(f)

    // Sort files either alphabetically or randomly using the provided seed:
    val sortedFiles = flatten(sourceDir).sorted
    val randomized  = randomOrder match {
      case None       => sortedFiles
      case Some(seed) => new Random(seed).shuffle(sortedFiles.toList).toArray
    }

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir + "/" + sourceDir.getName + "/")
    targetDir.mkdirs()

    val target = JointCompilationSource(s"compiling '$f' in test '$callingMethod'", randomized, flags, targetDir)
    new CompilationTest(target)
  }

  /** Compiles all `files` together as a single compilation run. It is given a
   *  `testName` since files can be in separate directories and or be otherwise
   *  dissociated
   */
  def compileList(testName: String, files: List[String], flags: Array[String], callingMethod: String = getCallingMethod())(implicit outDirectory: String): CompilationTest = {
    val outDir = outDirectory + callingMethod + "/" + testName + "/"

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir)
    targetDir.mkdirs()
    assert(targetDir.exists, s"couldn't create target directory: $targetDir")

    val target = JointCompilationSource(s"$testName from $callingMethod", files.map(new JFile(_)).toArray, flags, targetDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(target)
  }

  /** This function compiles the files and folders contained within directory
   *  `f` in a specific way.
   *
   *  - Each file is compiled separately as a single compilation run
   *  - Each directory is compiled as a `SeparateCompilationTaret`, in this
   *    target all files are grouped according to the file suffix `_X` where `X`
   *    is a number. These groups are then ordered in ascending order based on
   *    the value of `X` and each group is compiled one after the other.
   *
   *  For this function to work as expected, we use the same convention for
   *  directory layout as the old partest. That is:
   *
   *  - Single files can have an associated check-file with the same name (but
   *    with file extension `.check`)
   *  - Directories can have an associated check-file, where the check file has
   *    the same name as the directory (with the file extension `.check`)
   */
  def compileFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val callingMethod = getCallingMethod()
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir)

    val targets =
      files.map(f => JointCompilationSource(callingMethod, Array(f), flags, createOutputDirsForFile(f, sourceDir, outDir))) ++
      dirs.map(dir => SeparateCompilationSource(callingMethod, dir, flags, createOutputDirsForDir(dir, sourceDir, outDir)))

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }

  /** This function behaves similar to `compileFilesInDir` but it ignores
   *  sub-directories and as such, does **not** perform separate compilation
   *  tests.
   */
  def compileShallowFilesInDir(f: String, flags: Array[String])(implicit outDirectory: String): CompilationTest = {
    val callingMethod = getCallingMethod()
    val outDir = outDirectory + callingMethod + "/"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (_, files) = compilationTargets(sourceDir)

    val targets = files.map { file =>
      JointCompilationSource(callingMethod, Array(file), flags, createOutputDirsForFile(file, sourceDir, outDir))
    }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }
}

object ParallelTesting {
  def isSourceFile(f: JFile): Boolean = {
    val name = f.getName
    name.endsWith(".scala") || name.endsWith(".java")
  }
}
