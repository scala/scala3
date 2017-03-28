package dotty
package tools
package dotc

import java.io.{ File => JFile }
import java.text.SimpleDateFormat
import java.util.HashMap
import java.lang.reflect.InvocationTargetException
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{ Files, Path, Paths, NoSuchFileException }
import java.util.concurrent.{ Executors => JExecutors, TimeUnit, TimeoutException }

import scala.io.Source
import scala.util.control.NonFatal
import scala.util.Try
import scala.collection.mutable
import scala.util.matching.Regex

import core.Contexts._
import reporting.{ Reporter, TestReporter }
import reporting.diagnostic.MessageContainer
import interfaces.Diagnostic.ERROR
import dotc.util.DiffUtil

/** A parallel testing suite whose goal is to integrate nicely with JUnit
 *
 *  This trait can be mixed in to offer parallel testing to compile runs. When
 *  using this, you should be running your JUnit tests **sequentially**, as the
 *  test suite itself runs with a high level of concurrency.
 */
trait ParallelTesting {

  /** If the running environment supports an interactive terminal, each `Test`
   *  will be run with a progress bar and real time feedback
   */
  def interactive: Boolean

  /** A regex denoting which tests to run, please note that this gets turned
   *  into an actual regular expression - and as such should be valid.
   *
   *  E.g: `*`is not a regular expression, but `.*` is.
   */
  def regex: Option[String]

  /** The `regex` string gets turned into an actual regular expression used for
   *  filtering tests
   */
  private lazy val filter: Option[Regex] = regex.map(str => new Regex(str))

  /** A compilation target whose files or directory of files is to be compiled
   *  in a specific way defined by the `Test`
   */
  private sealed trait Target { self =>
    def outDir: JFile
    def flags: Array[String]

    /** Adds the flags specified in `newFlags0` if they do not already exist */
    def withFlags(newFlags0: String*) = {
      val newFlags = newFlags0.toArray
      if (!flags.containsSlice(newFlags)) self match {
        case self: ConcurrentCompilationTarget =>
          self.copy(flags = flags ++ newFlags)
        case self: SeparateCompilationTarget =>
          self.copy(flags = flags ++ newFlags)
      }
      else self
    }

    /** Generate the instructions to redo the test from the command line */
    def buildInstructions(errors: Int, warnings: Int): String = {
      val sb = new StringBuilder
      val maxLen = 80
      var lineLen = 0

      sb.append(s"\n\nTest compiled with $errors error(s) and $warnings warning(s), the test can be reproduced by running:")
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
        case ConcurrentCompilationTarget(files, _, _) => {
          files.map(_.getAbsolutePath).foreach { path =>
            sb.append("\\\n        ")
            sb.append(path)
            sb += ' '
          }
          sb.toString + "\n\n"
        }
        case self: SeparateCompilationTarget => {
          val command = sb.toString
          val fsb = new StringBuilder(command)
          self.compilationUnits.foreach { files =>
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
  private final case class ConcurrentCompilationTarget(
    files: Array[JFile],
    flags: Array[String],
    outDir: JFile
  ) extends Target {
    override def toString() = outDir.toString
  }

  /** A target whose files will be compiled separately according to their
   *  suffix `_X`
   */
  private final case class SeparateCompilationTarget(
    dir: JFile,
    flags: Array[String],
    outDir: JFile
  ) extends Target {

    /** Get the files grouped by `_X` as a list of groups, files missing this
     *  suffix will be put into the same group
     */
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

  /** Each `Test` takes the `targets` and performs the compilation and assertions
   *  according to the implementing class "neg", "run" or "pos".
   */
  private abstract class Test(targets: List[Target], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean) {

    /** Actual compilation run logic, the test behaviour is defined here */
    protected def compilationRunnable(target: Target): Runnable

    /** All targets left after filtering out */
    private val allTargets =
      if (!filter.isDefined) targets
      else targets.filter {
        case ConcurrentCompilationTarget(files, _, _) =>
          files.exists(file => filter.get.findFirstIn(file.getAbsolutePath).isDefined)
        case SeparateCompilationTarget(dir, _, _) =>
          filter.get.findFirstIn(dir.getAbsolutePath).isDefined
      }

    /** Total amount of targets being compiled by this test */
    val totalTargets = allTargets.length

    private[this] var _errors =  0
    def errors: Int = synchronized { _errors }

    private[this] var _targetsCompiled = 0
    private def targetsCompiled: Int = synchronized { _targetsCompiled }

    /** Complete the current compilation with the amount of errors encountered */
    protected final def completeCompilation(errors: Int) = synchronized {
      _targetsCompiled += 1
      _errors += errors
    }

    private[this] var _failed = false
    /** Fail the current test */
    protected[this] final def fail(): Unit = synchronized { _failed = true }
    def didFail: Boolean = _failed

    /** Instructions on how to reproduce failed target compilations */
    private[this] val failureInstructions = mutable.ArrayBuffer.empty[String]
    protected final def addFailureInstruction(ins: String): Unit =
      synchronized { failureInstructions.append(ins) }

    /** The targets that failed according to the implementing subclass */
    private[this] val failedCompilationTargets = mutable.ArrayBuffer.empty[String]
    protected final def addFailedCompilationTarget(target: String): Unit =
      synchronized { failedCompilationTargets.append(target) }

    /** Fails the current target, and makes sure it gets logged */
    protected final def failTarget(target: Target) =
      target match {
        case ConcurrentCompilationTarget(files, _, _) =>
          files.map(_.getAbsolutePath).foreach(addFailedCompilationTarget)
          fail()
        case SeparateCompilationTarget(dir, _, _) =>
          addFailedCompilationTarget(dir.getAbsolutePath)
          fail()
      }

    /** Prints to `System.err` if we're not suppressing all output */
    protected def echo(msg: String): Unit =
      if (!suppressAllOutput) System.err.println(msg)

    /** A single `Runnable` that prints a progress bar for the curent `Test` */
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

    /** Wrapper function to make sure that the compiler itself did not crash -
     *  if it did, the test should automatically fail.
     */
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

    protected def compile(files0: Array[JFile], flags0: Array[String], suppressErrors: Boolean, targetDir: JFile): TestReporter = {

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
        val scalaLib = findJarFromRuntime("scala-library-2.")
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
      val reporter = TestReporter.parallelReporter(this, logLevel =
        if (suppressErrors || suppressAllOutput) ERROR + 1 else ERROR)
      val driver =
        if (times == 1) new Driver { def newCompiler(implicit ctx: Context) = new Compiler }
        else new Driver {
          def newCompiler(implicit ctx: Context) = new Compiler

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
      driver.process(allArgs ++ files.map(_.getAbsolutePath), reporter = reporter)

      // If the java files failed compilation before, we try again after:
      if (!javaCompiledBefore)
        assert(compileWithJavac(javaFiles), s"java compilation failed for ${javaFiles.mkString(", ")}")

      reporter
    }

    private[ParallelTesting] def execute(): this.type = {
      assert(_targetsCompiled == 0, "not allowed to re-use a `CompileRun`")

      if (allTargets.nonEmpty) {
        val pool = threadLimit match {
          case Some(i) => JExecutors.newWorkStealingPool(i)
          case None => JExecutors.newWorkStealingPool()
        }

        if (interactive && !suppressAllOutput) pool.submit(statusRunner)

        allTargets.foreach { target =>
          pool.submit(compilationRunnable(target))
        }

        pool.shutdown()
        if (!pool.awaitTermination(10, TimeUnit.MINUTES))
          throw new TimeoutException("Compiling targets timed out")

        if (didFail) {
          echo {
            """|
               |================================================================================
               |Test Report
               |================================================================================
               |Failing tests:""".stripMargin
          }
          failedCompilationTargets.toArray.sorted.foreach(echo)
          failureInstructions.iterator.foreach(echo)
        }
      }
      else echo {
        regex
          .map(r => s"""No files matched regex "$r" in test""")
          .getOrElse("No tests available under target - erroneous test?")
      }

      this
    }
  }

  @inline private final def isCompilable(f: JFile): Boolean = {
    val name = f.getName
    name.endsWith(".scala") || name.endsWith(".java")
  }

  private final class PosTest(targets: List[Target], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)
  extends Test(targets, times, threadLimit, suppressAllOutput) {
    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        target match {
          case ConcurrentCompilationTarget(files, flags, outDir) => {
            val sourceFiles = files.filter(isCompilable)
            val reporter = compile(sourceFiles, flags, false, outDir)
            completeCompilation(reporter.errorCount)

            if (reporter.errorCount > 0) {
              fail()
              val errorMsg = target.buildInstructions(reporter.errorCount, reporter.warningCount)
              addFailureInstruction(errorMsg)
              failTarget(target)
              reporter.echo(errorMsg)
              reporter.flushToFile()
            }
          }

          case target @ SeparateCompilationTarget(dir, flags, outDir) => {
            val compilationUnits = target.compilationUnits
            val reporters = compilationUnits.map(files => compile(files.filter(isCompilable), flags, false, outDir))
            val errorCount = reporters.foldLeft(0) { (acc, reporter) =>
              if (reporter.errorCount > 0) {
                val errorMsg = target.buildInstructions(reporter.errorCount, reporter.warningCount)
                addFailureInstruction(errorMsg)
                reporter.echo(errorMsg)
                reporter.flushToFile()
              }

              acc + reporter.errorCount
            }

            completeCompilation(errorCount)

            if (errorCount > 0) failTarget(target)
          }
        }

      }
    }
  }

  private final class RunTest(targets: List[Target], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)
  extends Test(targets, times, threadLimit, suppressAllOutput) {
    private def verifyOutput(checkFile: JFile, dir: JFile, target: Target, warnings: Int) = try {
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
          // Print diff to files and summary:
          val diff = outputLines.zip(checkLines).map { case (act, exp) =>
            DiffUtil.mkColoredCodeDiff(exp, act, true)
          }.mkString("\n")
          val msg = s"\nOutput from run test '$checkFile' did not match expected, output:\n$diff\n"
          echo(msg)
          addFailureInstruction(msg)

          // Print build instructions to file and summary:
          val buildInstr = target.buildInstructions(0, warnings)
          addFailureInstruction(buildInstr)

          // Fail target:
          failTarget(target)
        }
    }
    catch {
      case _: NoSuchMethodException =>
        echo(s"\ntest in '$dir' did not contain a main method")
        fail()

      case _: ClassNotFoundException =>
        echo(s"\ntest in '$dir' did was not contained within a `Test` object")
        fail()

      case _: InvocationTargetException =>
        echo(s"\nTest in '$dir' might be using args(X) where X > 0")
        fail()
    }

    protected def compilationRunnable(target: Target): Runnable = new Runnable {
      def run(): Unit = compileTry {
        val (errorCount, warningCount, hasCheckFile, doVerify) = target match {
          case ConcurrentCompilationTarget(files, flags, outDir) => {
            val sourceFiles = files.filter(isCompilable)
            val checkFile = files.flatMap { file =>
              if (file.isDirectory) Nil
              else {
                val fname = file.getAbsolutePath.reverse.dropWhile(_ != '.').reverse + "check"
                val checkFile = new JFile(fname)
                if (checkFile.exists) List(checkFile)
                else Nil
              }
            }.headOption
            val reporter = compile(sourceFiles, flags, false, outDir)

            if (reporter.errorCount > 0) {
              fail()
              val errorMsg = target.buildInstructions(reporter.errorCount, reporter.warningCount)
              addFailureInstruction(errorMsg)
              failTarget(target)
              reporter.echo(errorMsg)
              reporter.flushToFile()
            }

            completeCompilation(reporter.errorCount)
            (reporter.errorCount, reporter.warningCount, checkFile.isDefined, () => verifyOutput(checkFile.get, outDir, target, reporter.warningCount))
          }

          case target @ SeparateCompilationTarget(dir, flags, outDir) => {
            val checkFile = new JFile(dir.getAbsolutePath.reverse.dropWhile(_ == '/').reverse + ".check")
            val (errorCount, warningCount) =
              target
                .compilationUnits
                .map(files => compile(files.filter(isCompilable), flags, false, outDir))
                .foldLeft((0,0)) { case ((errors, warnings), reporter) =>
                  if (reporter.errorCount > 0) {
                    val errorMsg = target.buildInstructions(reporter.errorCount, reporter.warningCount)
                    addFailureInstruction(errorMsg)
                    reporter.echo(errorMsg)
                    reporter.flushToFile()
                  }

                  (errors + reporter.errorCount, warnings + reporter.warningCount)
                }

            if (errorCount > 0) fail()

            completeCompilation(errorCount)
            (errorCount, warningCount, checkFile.exists, () => verifyOutput(checkFile, outDir, target, warningCount))
          }
        }

        if (errorCount == 0 && hasCheckFile) doVerify()
        else if (errorCount > 0) {
          echo(s"\nCompilation failed for: '$target'")
          val buildInstr = target.buildInstructions(errorCount, warningCount)
          addFailureInstruction(buildInstr)
          failTarget(target)
        }
      }
    }
  }

  private final class NegTest(targets: List[Target], times: Int, threadLimit: Option[Int], suppressAllOutput: Boolean)
  extends Test(targets, times, threadLimit, suppressAllOutput) {
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

        def getMissingAnnotations(errorMap: HashMap[String, Integer], reporterErrors: Iterator[MessageContainer]) = !reporterErrors.forall { error =>
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
            echo {
              s"Error reported in ${error.pos.source}, but no annotation found"
            }
            false
          }
        }

        val (expectedErrors, actualErrors, hasMissingAnnotations, errorMap) = target match {
          case ConcurrentCompilationTarget(files, flags, outDir) => {
            val sourceFiles = files.filter(isCompilable)
            val (errorMap, expectedErrors) = errorMapAndExpected(sourceFiles)
            val reporter = compile(sourceFiles, flags, true, outDir)
            val actualErrors = reporter.errorCount

            (expectedErrors, actualErrors, () => getMissingAnnotations(errorMap, reporter.errors), errorMap)
          }

          case target @ SeparateCompilationTarget(dir, flags, outDir) => {
            val compilationUnits = target.compilationUnits
            val (errorMap, expectedErrors) = errorMapAndExpected(compilationUnits.toArray.flatten)
            val reporters = compilationUnits.map(files => compile(files.filter(isCompilable), flags, true, outDir))
            val actualErrors = reporters.foldLeft(0)(_ + _.errorCount)
            val errors = reporters.iterator.flatMap(_.errors)
            (expectedErrors, actualErrors, () => getMissingAnnotations(errorMap, errors), errorMap)
          }
        }

        if (expectedErrors != actualErrors) {
          echo {
            s"\nWrong number of errors encountered when compiling $target, expected: $expectedErrors, actual: $actualErrors\n"
          }
          failTarget(target)
        }
        else if (hasMissingAnnotations()) {
          echo {
            s"\nErrors found on incorrect row numbers when compiling $target"
          }
          failTarget(target)
        }
        else if (!errorMap.isEmpty) {
          echo {
            s"\nError annotation(s) have {<error position>=<unreported error>}: $errorMap"
          }
          failTarget(target)
        }

        completeCompilation(actualErrors)
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
   *  Pos tests verify that the compiler is able to compile the given `Target`s
   *  and that they generate no errors or exceptions during compilation
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
    private[ParallelTesting] val targets: List[Target],
    private[ParallelTesting] val times: Int,
    private[ParallelTesting] val shouldDelete: Boolean,
    private[ParallelTesting] val threadLimit: Option[Int],
    private[ParallelTesting] val shouldFail: Boolean
  ) {
    import org.junit.Assert.fail

    private[ParallelTesting] def this(target: Target) =
      this(List(target), 1, true, None, false)

    private[ParallelTesting] def this(targets: List[Target]) =
      this(targets, 1, true, None, false)

    /** Compose test targets from `this` with `other`
     *
     *  It does this, only if the two tests are compatible. Otherwise it throws
     *  an `IllegalArgumentException`.
     *
     *  Grouping tests together like this allows us to take advantage of the
     *  concurrency offered by this test suite as each call to an executing
     *  method (`pos()` / `neg()`/ `run()`) will spin up a thread pool with the
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
      new CompilationTest(targets ++ other.targets, times, shouldDelete, threadLimit, shouldFail)
    }

    /** Creates a "pos" test run, which makes sure that all tests pass
     *  compilation without generating errors and that they do not crash the
     *  compiler
     */
    def pos(): this.type = {
      val test = new PosTest(targets, times, threadLimit, shouldFail).execute()

      if (!shouldFail && test.didFail) {
        fail(s"Expected no errors when compiling, but found: ${test.errors}")
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
    def neg(): this.type = {
      val test = new NegTest(targets, times, threadLimit, shouldFail).execute()

      if (!shouldFail && test.didFail) {
        fail("Neg test shouldn't have failed, but did")
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
    def run(): this.type = {
      val test = new RunTest(targets, times, threadLimit, shouldFail).execute()

      if (!shouldFail && test.didFail) {
        fail("Run test failed, but should not")
      }
      else if (shouldFail && !test.didFail) {
        fail("Run test should have failed, but did not")
      }

      cleanup()
    }

    /** Deletes output directories and files */
    private def cleanup(): this.type = {
      if (shouldDelete) targets.foreach(t => delete(t.outDir))
      this
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
        case target @ ConcurrentCompilationTarget(files, _, outDir) =>
          target.copy(files = files.map(copyToDir(outDir,_)))
        case target @ SeparateCompilationTarget(dir, _, outDir) =>
          target.copy(dir = copyToDir(outDir, dir))
      },
      times, shouldDelete, threadLimit, shouldFail
    )

    /** Builds a `CompilationTest` which performs the compilation `i` times on
     *  each target
     */
    def times(i: Int): CompilationTest =
      new CompilationTest(targets, i, shouldDelete, threadLimit, shouldFail)

    /** Builds a `Compilationtest` which passes the verbose flag and logs the
     *  classpath
     */
    def verbose: CompilationTest = new CompilationTest(
      targets.map(t => t.withFlags("-verbose", "-Ylog-classpath")),
      times, shouldDelete, threadLimit, shouldFail
    )

    /** Builds a `CompilationTest` which keeps the generated output files
     *
     *  This is needed for tests like `tastyBootstrap` which relies on first
     *  compiling a certain part of the project and then compiling a second
     *  part which depends on the first
     */
    def keepOutput: CompilationTest =
      new CompilationTest(targets, times, false, threadLimit, shouldFail)

    /** Builds a `CompilationTest` with a limited level of concurrency with
     *  maximum `i` threads
     */
    def limitThreads(i: Int): CompilationTest =
      new CompilationTest(targets, times, shouldDelete, Some(i), shouldFail)

    /** Builds a `CompilationTest` where the executed test is expected to fail
     *
     *  This behaviour is mainly needed for the tests that test the test suite.
     */
    def expectFailure: CompilationTest =
      new CompilationTest(targets, times, shouldDelete, threadLimit, true)

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
  private def toCompilerDirFromDir(d: JFile, sourceDir: JFile, outDir: String): JFile = {
    val targetDir = new JFile(outDir + s"${sourceDir.getName}/${d.getName}")
    targetDir.mkdirs()
    targetDir
  }

  /** Create out directory for `file` */
  private def toCompilerDirFromFile(file: JFile, sourceDir: JFile, outDir: String): JFile = {
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
      else if (f.getName.endsWith(".check")) (dirs, files)
      else if (f.getName.endsWith(".flags")) (dirs, files)
      else (dirs, f :: files)
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

  /** Compiles a single file from the string path `f` using the supplied flags */
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

  /** Compiles a directory `f` using the supplied `flags`. This method does
   *  deep compilation, that is - it compiles all files and subdirectories
   *  contained within the directory `f`.
   */
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

  /** Compiles all `files` together as a single compilation run. It is given a
   *  `testName` since files can be in separate directories and or be otherwise
   *  dissociated
   */
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

  /** This function behaves similar to `compileFilesInDir` but it ignores
   *  sub-directories and as such, does **not** perform separate compilation
   *  tests.
   */
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
