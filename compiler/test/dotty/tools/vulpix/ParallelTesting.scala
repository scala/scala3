package dotty
package tools
package vulpix

import java.io.{File => JFile}
import java.lang.System.{lineSeparator => EOL}
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{Files, NoSuchFileException, Path, Paths}
import java.text.SimpleDateFormat
import java.util.{HashMap, Timer, TimerTask}
import java.util.concurrent.{TimeUnit, TimeoutException, Executors => JExecutors}

import scala.collection.mutable
import scala.io.Source
import scala.util.{Random, Try}
import scala.util.control.NonFatal
import scala.util.matching.Regex

import dotc.{Compiler, Driver}
import dotc.core.Contexts._
import dotc.decompiler
import dotc.interfaces.Diagnostic.ERROR
import dotc.reporting.{Reporter, TestReporter}
import dotc.reporting.diagnostic.MessageContainer
import dotc.util.DiffUtil
import dotty.tools.vulpix.TestConfiguration.defaultOptions

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

  /** Tests should override the checkfiles with the current output */
  def updateCheckFiles: Boolean

  /** A test source whose files or directory of files is to be compiled
   *  in a specific way defined by the `Test`
   */
  sealed trait TestSource { self =>
    def name: String
    def outDir: JFile
    def flags: TestFlags

    def runClassPath: String = outDir.getAbsolutePath + JFile.pathSeparator + flags.runClassPath

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
      if (!flags.options.containsSlice(newFlags)) self match {
        case self: JointCompilationSource =>
          self.copy(flags = flags.and(newFlags:_*))
        case self: SeparateCompilationSource =>
          self.copy(flags = flags.and(newFlags:_*))
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
      flags.all.foreach { arg =>
        if (lineLen > maxLen) {
          sb.append(" \\\n        ")
          lineLen = 4
        }
        sb.append(arg)
        lineLen += arg.length
        sb += ' '
      }

      self match {
        case source: JointCompilationSource => {
          source.sourceFiles.map(_.getAbsolutePath).foreach { path =>
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
  private case class JointCompilationSource(
    name: String,
    files: Array[JFile],
    flags: TestFlags,
    outDir: JFile,
    fromTasty: Boolean = false,
    decompilation: Boolean = false
  ) extends TestSource {
    def sourceFiles: Array[JFile] = files.filter(isSourceFile)

    override def toString() = outDir.toString
  }

  /** A test source whose files will be compiled separately according to their
   *  suffix `_X`
   */
  case class SeparateCompilationSource(
    name: String,
    dir: JFile,
    flags: TestFlags,
    outDir: JFile
  ) extends TestSource {

    /** Get the files grouped by `_X` as a list of groups, files missing this
     *  suffix will be put into the same group.
     *  Files in each group are sorted alphabetically.
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
      .toList.sortBy(_._1).map(_._2.filter(isSourceFile).sorted)
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
        case JointCompilationSource(_, files, _, _, _, _) =>
          files.exists(file => file.getAbsolutePath.contains(testFilter.get))
        case SeparateCompilationSource(_, dir, _, _) =>
          dir.getAbsolutePath.contains(testFilter.get)
      }

    /** Total amount of test sources being compiled by this test */
    val sourceCount = filteredSources.length

    private[this] var _testSourcesCompleted = 0
    private def testSourcesCompleted: Int = _testSourcesCompleted

    /** Complete the current compilation with the amount of errors encountered */
    protected final def registerCompletion() = synchronized {
      _testSourcesCompleted += 1
    }

    sealed trait Failure
    case class JavaCompilationFailure(reason: String) extends Failure
    case class TimeoutFailure(title: String) extends Failure
    case object Generic extends Failure

    private[this] var _failures = Set.empty[Failure]
    private[this] var _failureCount = 0

    /** Fail the current test */
    protected[this] final def fail(failure: Failure = Generic): Unit = synchronized {
      _failures = _failures + failure
      _failureCount = _failureCount + 1
    }
    def didFail: Boolean = _failureCount != 0

    /** A set of the different failures */
    def failureReasons: Set[Failure] = _failures

    /** Number of failed tests */
    def failureCount: Int = _failureCount

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

    /** Print a progress bar for the current `Test` */
    private def updateProgressMonitor(start: Long): Unit = {
      val tCompiled = testSourcesCompleted
      if (tCompiled < sourceCount) {
        val timestamp = (System.currentTimeMillis - start) / 1000
        val progress = (tCompiled.toDouble / sourceCount * 40).toInt

        realStdout.print(
          "[" + ("=" * (math.max(progress - 1, 0))) +
            (if (progress > 0) ">" else "") +
            (" " * (39 - progress)) +
            s"] completed ($tCompiled/$sourceCount, $failureCount failed, ${timestamp}s)\r"
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
          registerCompletion()
          throw e
        }
      }

    protected def compile(files0: Array[JFile], flags0: TestFlags, suppressErrors: Boolean, targetDir: JFile): TestReporter = {

      val flags = flags0.and("-d", targetDir.getAbsolutePath)
        .withClasspath(targetDir.getAbsolutePath)

      def flattenFiles(f: JFile): Array[JFile] =
        if (f.isDirectory) f.listFiles.flatMap(flattenFiles)
        else Array(f)

      val files: Array[JFile] = files0.flatMap(flattenFiles)

      def compileWithJavac(fs: Array[String]) = if (fs.nonEmpty) {
        val fullArgs = Array(
          "javac",
          "-encoding", "UTF-8",
        ) ++ flags.javacFlags ++ fs

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

      val allArgs = flags.all

      // Compile with a try to catch any StackTrace generated by the compiler:
      try {
        // If a test contains a Java file that cannot be parsed by Dotty's Java source parser, its
        // name must contain the string "JAVA_ONLY".
        val dottyFiles = files.filterNot(_.getName.contains("JAVA_ONLY")).map(_.getAbsolutePath)
        driver.process(allArgs ++ dottyFiles, reporter = reporter)

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

    protected def compileFromTasty(flags0: TestFlags, suppressErrors: Boolean, targetDir: JFile): TestReporter = {
      val tastyOutput = new JFile(targetDir.getPath + "_from-tasty")
      tastyOutput.mkdir()
      val flags = flags0 and ("-d", tastyOutput.getAbsolutePath) and "-from-tasty"

      def tastyFileToClassName(f: JFile): String = {
        val pathStr = targetDir.toPath.relativize(f.toPath).toString.replace(JFile.separatorChar, '.')
        pathStr.stripSuffix(".tasty").stripSuffix(".hasTasty")
      }
      val classes = flattenFiles(targetDir).filter(isTastyFile).map(tastyFileToClassName)

      val reporter =
        TestReporter.reporter(realStdout, logLevel =
          if (suppressErrors || suppressAllOutput) ERROR + 1 else ERROR)

      val driver = new Driver

      // Compile with a try to catch any StackTrace generated by the compiler:
      try {
        driver.process(flags.all ++ classes, reporter = reporter)
      }
      catch {
        case NonFatal(ex) => reporter.logStackTrace(ex)
      }

      reporter
    }

    protected def decompile(flags0: TestFlags, suppressErrors: Boolean, targetDir0: JFile): TestReporter = {
      val targetDir = new JFile(targetDir0.getParent + "_decompiled")
      val decompilationOutput = new JFile(targetDir + JFile.separator + targetDir0.getName)
      decompilationOutput.mkdirs()
      val flags =
        flags0 and ("-d", decompilationOutput.getAbsolutePath) and
        "-decompile" and "-pagewidth" and "80"

      def hasTastyFileToClassName(f: JFile): String =
        targetDir0.toPath.relativize(f.toPath).toString.stripSuffix(".hasTasty").
          stripSuffix(".tasty").replace(JFile.separatorChar, '.')
      val classes = flattenFiles(targetDir0).filter(isTastyFile).map(hasTastyFileToClassName).sorted

      val reporter =
        TestReporter.reporter(realStdout, logLevel =
          if (suppressErrors || suppressAllOutput) ERROR + 1 else ERROR)

      val driver = decompiler.Main

      // Compile with a try to catch any StackTrace generated by the compiler:
      try {
        driver.process(flags.all ++ classes, reporter = reporter)
      }
      catch {
        case NonFatal(ex) => reporter.logStackTrace(ex)
      }

      reporter
    }

    private[ParallelTesting] def executeTestSuite(): this.type = {
      assert(testSourcesCompleted == 0, "not allowed to re-use a `CompileRun`")

      if (filteredSources.nonEmpty) {
        val pool = threadLimit match {
          case Some(i) => JExecutors.newWorkStealingPool(i)
          case None => JExecutors.newWorkStealingPool()
        }

        val timer = new Timer()
        val logProgress = isInteractive && !suppressAllOutput
        val start = System.currentTimeMillis()
        if (logProgress) {
          val task = new TimerTask {
            def run(): Unit = updateProgressMonitor(start)
          }
          timer.schedule(task, 100, 200)
        }

        val eventualResults = filteredSources.map { target =>
          pool.submit(encapsulatedCompilation(target))
        }

        pool.shutdown()
        if (!pool.awaitTermination(20, TimeUnit.MINUTES)) {
          pool.shutdownNow()
          System.setOut(realStdout)
          System.setErr(realStderr)
          throw new TimeoutException("Compiling targets timed out")
        }

        eventualResults.foreach { x =>
          try x.get()
          catch {
            case ex: Exception =>
              System.err.println(ex.getMessage)
              ex.printStackTrace()
          }
        }

        if (logProgress) {
          timer.cancel()
          val timestamp = (System.currentTimeMillis - start) / 1000
          realStdout.println(
            s"[=======================================] completed ($sourceCount/$sourceCount, $failureCount failed, ${timestamp}s)"
          )
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

    protected def updateCheckFile(checkFile: JFile, lines: Seq[String]): Unit = {
      val outFile = dotty.tools.io.File(checkFile.toPath)
      outFile.writeAll(lines.mkString("", EOL, EOL))
      echo("Updated checkfile: " + checkFile.getPath)
    }

    /** Returns all files in directory or the file if not a directory */
    private def flattenFiles(f: JFile): Array[JFile] =
      if (f.isDirectory) f.listFiles.flatMap(flattenFiles)
      else Array(f)
  }

  private final class PosTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
  extends Test(testSources, times, threadLimit, suppressAllOutput) {
    protected def encapsulatedCompilation(testSource: TestSource) = new LoggedRunnable {
      def checkTestSource(): Unit = tryCompile(testSource) {
        testSource match {
          case testSource @ JointCompilationSource(name, files, flags, outDir, fromTasty, decompilation) =>
            val reporter =
              if (decompilation) {
                val rep = decompile(flags, false, outDir)

                val checkFileOpt = files.flatMap { file =>
                  if (file.isDirectory) Nil
                  else {
                    val fname = file.getAbsolutePath.reverse.dropWhile(_ != '.').reverse + "decompiled"
                    val checkFile = new JFile(fname)
                    if (checkFile.exists) List(checkFile)
                    else Nil
                  }
                }.headOption
                checkFileOpt match {
                  case Some(checkFile) =>
                    val ignoredFilePathLine = "/** Decompiled from"
                    val stripTrailingWhitespaces = "(.*\\S|)\\s+".r
                    val output = Source.fromFile(outDir.getParent + "_decompiled" + JFile.separator + outDir.getName
                      + JFile.separator + "decompiled.scala", "UTF-8").getLines().map {line =>
                      stripTrailingWhitespaces.unapplySeq(line).map(_.head).getOrElse(line)
                    }.filter(!_.startsWith(ignoredFilePathLine)).toList

                    val check: String = Source.fromFile(checkFile, "UTF-8").getLines()
                      .mkString(EOL)

                    if (output.mkString(EOL) != check) {
                      val outFile = dotty.tools.io.File(checkFile.toPath).addExtension(".out")
                      if (updateCheckFiles) {
                        updateCheckFile(checkFile, output)
                      } else {
                        outFile.writeAll(output.mkString("", EOL, ""))
                        val msg =
                          s"""Output differed for test $name, use the following command to see the diff:
                             |  > diff $checkFile $outFile
                        """.stripMargin

                        echo(msg)
                        addFailureInstruction(msg)

                        // Print build instructions to file and summary:
                        val buildInstr = testSource.buildInstructions(0, rep.warningCount)
                        addFailureInstruction(buildInstr)

                        // Fail target:
                        failTestSource(testSource)
                      }
                    }
                  case _ =>
                }

                rep
              }
              else if (fromTasty) compileFromTasty(flags, false, outDir)
              else compile(testSource.sourceFiles, flags, false, outDir)
            registerCompletion()

            if (reporter.compilerCrashed || reporter.errorCount > 0) {
              logReporterContents(reporter)
              logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)
            }

          case testSource @ SeparateCompilationSource(_, dir, flags, outDir) =>
            val reporters = testSource.compilationGroups.map(files => compile(files, flags, false, outDir))
            val compilerCrashed = reporters.exists(_.compilerCrashed)
            val errorCount = reporters.foldLeft(0) { (acc, reporter) =>
              if (reporter.errorCount > 0)
                logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)

              acc + reporter.errorCount
            }

            def warningCount = reporters.foldLeft(0)(_ + _.warningCount)

            registerCompletion()

            if (compilerCrashed || errorCount > 0) {
              reporters.foreach(logReporterContents)
              logBuildInstructions(reporters.head, testSource, errorCount, warningCount)
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
          val outputLines = output.linesIterator.toSeq
          val checkLines: Seq[String] = Source.fromFile(checkFile.get, "UTF-8").getLines().toSeq
          val sourceTitle = testSource.title

          diffMessage(sourceTitle, outputLines, checkLines).foreach { msg =>

            echo(msg)
            addFailureInstruction(msg)

            // Print build instructions to file and summary:
            val buildInstr = testSource.buildInstructions(0, warnings)
            addFailureInstruction(buildInstr)

            // Fail target:
            failTestSource(testSource)

            if (updateCheckFiles)
              updateCheckFile(checkFile.get, outputLines)
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
          case testSource @ JointCompilationSource(_, files, flags, outDir, fromTasty, decompilation) =>
            val checkFile = files.flatMap { file =>
              if (file.isDirectory) Nil
              else {
                val fname = file.getAbsolutePath.reverse.dropWhile(_ != '.').reverse + "check"
                val checkFile = new JFile(fname)
                if (checkFile.exists) List(checkFile)
                else Nil
              }
            }.headOption
            val reporter =
              if (fromTasty) compileFromTasty(flags, false, outDir)
              else compile(testSource.sourceFiles, flags, false, outDir)

            if (reporter.compilerCrashed || reporter.errorCount > 0) {
              logReporterContents(reporter)
              logBuildInstructions(reporter, testSource, reporter.errorCount, reporter.warningCount)
            }

            (reporter.compilerCrashed, reporter.errorCount, reporter.warningCount, () => verifyOutput(checkFile, outDir, testSource, reporter.warningCount))

          case testSource @ SeparateCompilationSource(_, dir, flags, outDir) =>
            val checkFile = new JFile(dir.getAbsolutePath.reverse.dropWhile(_ == JFile.separatorChar).reverse + ".check")
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

        if (!compilerCrashed && errorCount == 0) verifier()
        else {
          echo(s"    Compilation failed for: '${testSource.title}'                               ")
          val buildInstr = testSource.buildInstructions(errorCount, warningCount)
          addFailureInstruction(buildInstr)
          failTestSource(testSource)
        }
        registerCompletion()
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
            Source.fromFile(file, "UTF-8").getLines().zipWithIndex.foreach { case (line, lineNbr) =>
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
            echo(s"Error reported in ${error.pos.source}, but no annotation found")
            false
          }
        }

        def fail(msg: String): Unit = {
          echo(msg)
          failTestSource(testSource)
        }

        def reporterOutputLines(reporters: List[TestReporter]): List[String] = {
          reporters.flatMap(_.allErrors).sortBy(_.pos.source.toString).flatMap { error =>
            (error.pos.span.toString + " in " + error.pos.source.file.name) :: error.getMessage().linesIterator.toList
          }
        }
        def checkFileTest(sourceName: String, checkFile: JFile, actual: List[String]) = {
          val expexted = Source.fromFile(checkFile, "UTF-8").getLines().toList
          for (msg <- diffMessage(sourceName, actual, expexted)) {
            fail(msg)
            if (updateCheckFiles)
              updateCheckFile(checkFile, actual)
          }
        }

        val (compilerCrashed, expectedErrors, actualErrors, hasMissingAnnotations, errorMap) = testSource match {
          case testSource @ JointCompilationSource(_, files, flags, outDir, fromTasty, decompilation) =>
            val sourceFiles = testSource.sourceFiles
            val (errorMap, expectedErrors) = getErrorMapAndExpectedCount(sourceFiles)
            val reporter = compile(sourceFiles, flags, true, outDir)
            val actualErrors = reporter.errorCount
            files.foreach { file =>
              if (!file.isDirectory) {
                val checkFile = new JFile(file.getAbsolutePath.replaceFirst("\\.scala$", ".check"))
                if (checkFile.exists)
                  checkFileTest(testSource.title, checkFile, reporterOutputLines(reporter :: Nil))
              }
            }
            if (reporter.compilerCrashed || actualErrors > 0)
              logReporterContents(reporter)

            (reporter.compilerCrashed, expectedErrors, actualErrors, () => getMissingExpectedErrors(errorMap, reporter.errors), errorMap)

          case testSource @ SeparateCompilationSource(_, dir, flags, outDir) => {
            val compilationGroups = testSource.compilationGroups
            val (errorMap, expectedErrors) = getErrorMapAndExpectedCount(compilationGroups.toArray.flatten)
            val reporters = compilationGroups.map(compile(_, flags, true, outDir))
            val compilerCrashed = reporters.exists(_.compilerCrashed)
            val actualErrors = reporters.foldLeft(0)(_ + _.errorCount)
            val errors = reporters.iterator.flatMap(_.errors)

            if (actualErrors > 0)
              reporters.foreach(logReporterContents)

            val checkFile = new JFile(dir.getAbsolutePath + ".check")
            if (checkFile.exists)
              checkFileTest(testSource.title, checkFile, reporterOutputLines(reporters))

            (compilerCrashed, expectedErrors, actualErrors, () => getMissingExpectedErrors(errorMap, errors), errorMap)
          }
        }


        if (compilerCrashed)
          fail(s"Compiler crashed when compiling: ${testSource.title}")
        else if (actualErrors == 0)
          fail(s"\nNo errors found when compiling neg test $testSource")
        else if (expectedErrors != actualErrors)
          fail(s"\nWrong number of errors encountered when compiling $testSource, expected: $expectedErrors, actual: $actualErrors")
        else if (hasMissingAnnotations())
          fail(s"\nErrors found on incorrect row numbers when compiling $testSource")
        else if (!errorMap.isEmpty)
          fail(s"\nExpected error(s) have {<error position>=<unreported error>}: $errorMap")

        registerCompletion()
      }
    }
  }

  private final class NoCrashTest(testSources: List[TestSource], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)(implicit summaryReport: SummaryReporting)
    extends Test(testSources, times, threadLimit, suppressAllOutput) {
    protected def encapsulatedCompilation(testSource: TestSource) = new LoggedRunnable {
      def checkTestSource(): Unit = tryCompile(testSource) {
        def fail(msg: String): Nothing = {
          echo(msg)
          failTestSource(testSource)
          ???
        }
        testSource match {
          case testSource@JointCompilationSource(_, files, flags, outDir, fromTasty, decompilation) =>
            val sourceFiles = testSource.sourceFiles
            val reporter =
              try compile(sourceFiles, flags, true, outDir)
              catch {
                case ex: Throwable => fail(s"Fatal compiler crash when compiling: ${testSource.title}")
              }
            if (reporter.compilerCrashed)
              fail(s"Compiler crashed when compiling: ${testSource.title}")
          case testSource@SeparateCompilationSource(_, dir, flags, outDir) => unsupported("NoCrashTest - SeparateCompilationSource")
        }
        registerCompletion()
      }
    }
  }

  def diffMessage(sourceTitle: String, outputLines: Seq[String], checkLines: Seq[String]): Option[String] = {
    def linesMatch =
      (outputLines, checkLines).zipped.forall(_ == _)

    if (outputLines.length != checkLines.length || !linesMatch) {
      // Print diff to files and summary:
      val diff = DiffUtil.mkColoredLineDiff(checkLines :+ DiffUtil.EOF, outputLines :+ DiffUtil.EOF)

      Some(
        s"""|Output from '$sourceTitle' did not match check file.
            |Diff (expected on the left, actual right):
            |""".stripMargin + diff + "\n")
    } else None

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
   *  compileFile("tests/pos/i1103.scala", opts).pos()
   *  ```
   *
   *  These tests can be customized before calling one of the execution
   *  methods, for instance:
   *
   *  ```
   *  compileFile("tests/pos/i1103.scala", opts).times(2).verbose.pos()
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
   *  crash the compiler. In each `.scala` file, you specify the line on which
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

    def this(target: TestSource) =
      this(List(target), 1, true, None, false, false)

    def this(targets: List[TestSource]) =
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

      cleanup()

      if (!shouldFail && test.didFail) {
        fail(s"Expected no errors when compiling, failed for the following reason(s):\n${ reasonsForFailure(test) }")
      }
      else if (shouldFail && !test.didFail) {
        fail("Pos test should have failed, but didn't")
      }

      this
    }

    /** Creates a "neg" test run, which makes sure that each test generates the
     *  correct amount of errors at the correct positions. It also makes sure
     *  that none of these tests crash the compiler
     */
    def checkExpectedErrors()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new NegTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput).executeTestSuite()

      cleanup()

      if (shouldFail && !test.didFail) {
        fail(s"Neg test shouldn't have failed, but did. Reasons:\n${ reasonsForFailure(test) }")
      }
      else if (!shouldFail && test.didFail) {
        fail("Neg test should have failed, but did not")
      }

      this
    }

    /** Creates a "fuzzy" test run, which makes sure that each test compiles (or not) without crashing */
    def checkNoCrash()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new NoCrashTest(targets, times, threadLimit, shouldSuppressOutput).executeTestSuite()

      cleanup()

      if (test.didFail) {
        fail("Neg test should have failed, but did not")
      }

      this
    }

    /** Creates a "run" test run, which is a superset of "pos". In addition to
     *  making sure that all tests pass compilation and that they do not crash
     *  the compiler; it also makes sure that all tests can run with the
     *  expected output
     */
    def checkRuns()(implicit summaryReport: SummaryReporting): this.type = {
      val test = new RunTest(targets, times, threadLimit, shouldFail || shouldSuppressOutput).executeTestSuite()

      cleanup()

      if (!shouldFail && test.didFail) {
        fail(s"Run test failed, but should not, reasons:\n${ reasonsForFailure(test) }")
      }
      else if (shouldFail && !test.didFail) {
        fail("Run test should have failed, but did not")
      }

      this
    }

    /** Deletes output directories and files */
    private def cleanup(): this.type = {
      if (shouldDelete) delete()
      this
    }

    /** Extract `Failure` set and render from `Test` */
    private[this] def reasonsForFailure(test: Test): String = {
      val failureReport =
        if (test.failureCount == 0) ""
        else s"\n  - encountered ${test.failureCount} test failures(s)"

      failureReport + test.failureReasons.collect {
        case test.TimeoutFailure(title) =>
          s"  - test '$title' timed out"
        case test.JavaCompilationFailure(msg) =>
          s"  - java compilation failed with:\n${ msg.linesIterator.map("      " + _).mkString("\n") }"
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
        case target @ JointCompilationSource(_, files, _, outDir, _, _) =>
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
  def createOutputDirsForDir(d: JFile, sourceDir: JFile, outDir: String): JFile = {
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/${d.getName}")
    targetDir.mkdirs()
    targetDir
  }

  /** Create out directory for `file` */
  private def createOutputDirsForFile(file: JFile, sourceDir: JFile, outDir: String): JFile = {
    val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
    val targetDir = new JFile(outDir + s"${sourceDir.getName}${JFile.separatorChar}$uniqueSubdir")
    targetDir.mkdirs()
    targetDir
  }

  /** Make sure that directory string is as expected */
  private def checkRequirements(f: String, sourceDir: JFile, outDir: String): Unit = {
    require(sourceDir.isDirectory && sourceDir.exists, "passed non-directory to `compileFilesInDir`: " + sourceDir)
    require(outDir.last == JFile.separatorChar, "please specify an `outDir` with a trailing file separator")
  }

  /** Separates directories from files and returns them as `(dirs, files)` */
  private def compilationTargets(sourceDir: JFile, fileFilter: FileFilter = FileFilter.NoFilter): (List[JFile], List[JFile]) =
    sourceDir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
      if (!fileFilter.accept(f.getName)) (dirs, files)
      else if (f.isDirectory) (f :: dirs, files)
      else if (isSourceFile(f)) (dirs, f :: files)
      else (dirs, files)
    }

  /** Compiles a single file from the string path `f` using the supplied flags */
  def compileFile(f: String, flags: TestFlags)(implicit testGroup: TestGroup): CompilationTest = {
    val sourceFile = new JFile(f)
    val parent = sourceFile.getParentFile
    val outDir =
      defaultOutputDir + testGroup + JFile.separator +
      sourceFile.getName.substring(0, sourceFile.getName.lastIndexOf('.')) + JFile.separator

    require(
      sourceFile.exists && !sourceFile.isDirectory &&
      (parent ne null) && parent.exists && parent.isDirectory,
      s"Source file: $f, didn't exist"
    )

    val target = JointCompilationSource(
      testGroup.name,
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
  def compileDir(f: String, flags: TestFlags, randomOrder: Option[Int] = None, recursive: Boolean = true)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    def flatten(f: JFile): Array[JFile] =
      if (f.isDirectory) {
        val files = f.listFiles
        if (recursive) files.flatMap(flatten) else files
      }
      else Array(f)

    // Sort files either alphabetically or randomly using the provided seed:
    val sortedFiles = flatten(sourceDir).sorted
    val randomized  = randomOrder match {
      case None       => sortedFiles
      case Some(seed) => new Random(seed).shuffle(sortedFiles.toList).toArray
    }

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir + JFile.separator + sourceDir.getName + JFile.separator)
    targetDir.mkdirs()

    val target = JointCompilationSource(s"compiling '$f' in test '$testGroup'", randomized, flags, targetDir)
    new CompilationTest(target)
  }

  /** Compiles all `files` together as a single compilation run. It is given a
   *  `testName` since files can be in separate directories and or be otherwise
   *  dissociated
   */
  def compileList(testName: String, files: List[String], flags: TestFlags)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator + testName + JFile.separator

    // Directories in which to compile all containing files with `flags`:
    val targetDir = new JFile(outDir)
    targetDir.mkdirs()
    assert(targetDir.exists, s"couldn't create target directory: $targetDir")

    val target = JointCompilationSource(s"$testName from $testGroup", files.map(new JFile(_)).toArray, flags, targetDir)

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(target)
  }

  /** This function compiles the files and folders contained within directory
   *  `f` in a specific way.
   *
   *  - Each file is compiled separately as a single compilation run
   *  - Each directory is compiled as a `SeparateCompilationTarget`, in this
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
  def compileFilesInDir(f: String, flags: TestFlags, fileFilter: FileFilter = FileFilter.NoFilter)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir, fileFilter)

    val isPicklerTest = flags.options.contains("-Ytest-pickler")
    def ignoreDir(dir: JFile): Boolean = {
      // Pickler tests stop after pickler not producing class/tasty files. The second part of the compilation
      // will not be able to compile due to the missing artifacts from the first part.
      isPicklerTest && dir.listFiles().exists(file => file.getName.endsWith("_2.scala") || file.getName.endsWith("_2.java"))
    }
    val targets =
      files.map(f => JointCompilationSource(testGroup.name, Array(f), flags, createOutputDirsForFile(f, sourceDir, outDir))) ++
      dirs.collect { case dir if !ignoreDir(dir) => SeparateCompilationSource(testGroup.name, dir, flags, createOutputDirsForDir(dir, sourceDir, outDir)) }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }

  /** This function compiles the files and folders contained within directory
   *  `f` in a specific way. Once compiled, they are recompiled/run from tasty as sources.
   *
   *  - Each file is compiled separately as a single compilation run
   *  - Each directory is compiled as a `SeparateCompilationTarget`, in this
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
   *
   *  Tests in the first part of the tuple must be executed before the second.
   *  Both testsRequires explicit delete().
   */
  def compileTastyInDir(f: String, flags0: TestFlags, fromTastyFilter: FileFilter, decompilationFilter: FileFilter, recompilationFilter: FileFilter)(
      implicit testGroup: TestGroup): TastyCompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val flags = flags0 and "-Yretain-trees"
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (dirs, files) = compilationTargets(sourceDir, fromTastyFilter)

    val filteredFiles = testFilter match {
      case Some(str) => files.filter(_.getAbsolutePath.contains(str))
      case None => files
    }

    class JointCompilationSourceFromTasty(
       name: String,
       file: JFile,
       flags: TestFlags,
       outDir: JFile,
       fromTasty: Boolean = false,
       decompilation: Boolean = false
    ) extends JointCompilationSource(name, Array(file), flags, outDir, fromTasty, decompilation) {

      override def buildInstructions(errors: Int, warnings: Int): String = {
        val runOrPos = if (file.getPath.startsWith(s"tests${JFile.separator}run${JFile.separator}")) "run" else "pos"
        val listName = if (fromTasty) "from-tasty" else "decompilation"
        s"""|
            |Test '$title' compiled with $errors error(s) and $warnings warning(s),
            |the test can be reproduced by running:
            |
            |  sbt "testCompilation --from-tasty $file"
            |
            |This tests can be disabled by adding `${file.getName}` to `compiler${JFile.separator}test${JFile.separator}dotc${JFile.separator}$runOrPos-$listName.blacklist`
            |
            |""".stripMargin
      }

    }

    val targets = filteredFiles.map { f =>
      val classpath = createOutputDirsForFile(f, sourceDir, outDir)
      new JointCompilationSourceFromTasty(testGroup.name, f, flags.withClasspath(classpath.getPath), classpath, fromTasty = true)
    }
    // TODO add SeparateCompilationSource from tasty?

    val targets2 =
      filteredFiles
        .filter(f => decompilationFilter.accept(f.getName))
        .map { f =>
          val classpath = createOutputDirsForFile(f, sourceDir, outDir)
          new JointCompilationSourceFromTasty(testGroup.name, f, flags.withClasspath(classpath.getPath), classpath, decompilation = true)
        }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    val generateClassFiles = compileFilesInDir(f, flags0, fromTastyFilter)

    val decompilationDir = outDir + sourceDir.getName + "_decompiled"

    if (targets2.isEmpty)
      new JFile(decompilationDir).mkdirs()

    new TastyCompilationTest(
      generateClassFiles.keepOutput,
      new CompilationTest(targets).keepOutput,
      new CompilationTest(targets2).keepOutput,
      recompilationFilter,
      decompilationDir,
      shouldDelete = true
    )
  }

  class TastyCompilationTest(step1: CompilationTest, step2: CompilationTest, step3: CompilationTest,
        recompilationFilter: FileFilter, decompilationDir: String, shouldDelete: Boolean)(implicit testGroup: TestGroup) {

    def keepOutput: TastyCompilationTest =
      new TastyCompilationTest(step1, step2, step3, recompilationFilter, decompilationDir, shouldDelete)

    def checkCompile()(implicit summaryReport: SummaryReporting): this.type = {
      step1.checkCompile() // Compile all files to generate the class files with tasty
      step2.checkCompile() // Compile from tasty
      step3.checkCompile() // Decompile from tasty

      val step4 = compileFilesInDir(decompilationDir, defaultOptions, recompilationFilter).keepOutput
      step4.checkCompile() // Recompile decompiled code

      if (shouldDelete)
        (step1 + step2 + step3 + step4).delete()

      this
    }

    def checkRuns()(implicit summaryReport: SummaryReporting): this.type = {
      step1.checkCompile() // Compile all files to generate the class files with tasty
      step2.checkRuns() // Compile from tasty
      step3.checkCompile() // Decompile from tasty

      val step4 = compileFilesInDir(decompilationDir, defaultOptions, recompilationFilter).keepOutput
      step4.checkRuns() // Recompile decompiled code

      if (shouldDelete)
        (step1 + step2 + step3 + step4).delete()

      this
    }
  }

  /** This function behaves similar to `compileFilesInDir` but it ignores
   *  sub-directories and as such, does **not** perform separate compilation
   *  tests.
   */
  def compileShallowFilesInDir(f: String, flags: TestFlags)(implicit testGroup: TestGroup): CompilationTest = {
    val outDir = defaultOutputDir + testGroup + JFile.separator
    val sourceDir = new JFile(f)
    checkRequirements(f, sourceDir, outDir)

    val (_, files) = compilationTargets(sourceDir)

    val targets = files.map { file =>
      JointCompilationSource(testGroup.name, Array(file), flags, createOutputDirsForFile(file, sourceDir, outDir))
    }

    // Create a CompilationTest and let the user decide whether to execute a pos or a neg test
    new CompilationTest(targets)
  }
}

object ParallelTesting {

  def defaultOutputDir: String = "out"+JFile.separator

  def isSourceFile(f: JFile): Boolean = {
    val name = f.getName
    name.endsWith(".scala") || name.endsWith(".java")
  }

  def isTastyFile(f: JFile): Boolean =
    f.getName.endsWith(".hasTasty") || f.getName.endsWith(".tasty")
}
