/* NOTE: Adapted from ScalaJSPartest.scala in
 * https://github.com/scala-js/scala-js/
 * TODO make partest configurable */

package dotty.partest

import scala.reflect.io.AbstractFile
import scala.tools.partest._
import scala.tools.partest.nest._
import TestState.{ Pass, Fail, Crash, Uninitialized, Updated }
import ClassPath.{ join, split }
import FileManager.{ compareFiles, compareContents, joinPaths, withTempFile }
import scala.util.matching.Regex
import tools.nsc.io.{ File => NSCFile }
import java.io.{ File, PrintStream, FileOutputStream, PrintWriter, FileWriter }
import java.net.URLClassLoader

/** Runs dotty partest from the Console, discovering test sources in
  * DPConfig.testRoot that have been generated automatically by
  * DPPrepJUnitRunner. Use `sbt partest` to run. If additional jars are
  * required by some run tests, add them to partestDeps in the sbt Build.scala.
  */
object DPConsoleRunner {
  def main(args: Array[String]): Unit = {
    // unfortunately sbt runTask passes args as single string
    // extra jars for run tests are passed with -dottyJars <count> <jar1> <jar2> ...
    val jarFinder = """-dottyJars (\d*) (.*)""".r
    val (jarList, otherArgs) = args.toList.partition(jarFinder.findFirstIn(_).isDefined)
    val (extraJars, moreArgs) = jarList match {
      case Nil => sys.error("Error: DPConsoleRunner needs \"-dottyJars <jarCount> <jars>*\".")
      case jarFinder(nr, jarString) :: Nil =>
        val jars = jarString.split(" ").toList
        val count = nr.toInt
        if (jars.length < count)
          sys.error("Error: DPConsoleRunner found wrong number of dottyJars: " + jars + ", expected: " + nr)
        else (jars.take(count), jars.drop(count))
      case list => sys.error("Error: DPConsoleRunner found several -dottyJars options: " + list)
    }
    new DPConsoleRunner((otherArgs ::: moreArgs) mkString (" "), extraJars).runPartest
  }
}

// console runner has a suite runner which creates a test runner for each test
class DPConsoleRunner(args: String, extraJars: List[String]) extends ConsoleRunner(args) {
  override val suiteRunner = new DPSuiteRunner (
    testSourcePath = optSourcePath getOrElse DPConfig.testRoot,
    fileManager = new DottyFileManager(extraJars),
    updateCheck = optUpdateCheck,
    failed = optFailed,
    consoleArgs = args)

  override def run = {}
  def runPartest = super.run
}

class DottyFileManager(extraJars: List[String]) extends FileManager(Nil) {
  lazy val extraJarList = extraJars.map(NSCFile(_))
  override lazy val libraryUnderTest  = Path(extraJars.find(_.contains("scala-library")).getOrElse(""))
  override lazy val reflectUnderTest  = Path(extraJars.find(_.contains("scala-reflect")).getOrElse(""))
  override lazy val compilerUnderTest = Path(extraJars.find(_.contains("dotty")).getOrElse(""))
}

class DPSuiteRunner(testSourcePath: String, // relative path, like "files", or "pending"
  fileManager: DottyFileManager,
  updateCheck: Boolean,
  failed: Boolean,
  consoleArgs: String,
  javaCmdPath: String = PartestDefaults.javaCmd,
  javacCmdPath: String = PartestDefaults.javacCmd,
  scalacExtraArgs: Seq[String] = Seq.empty,
  javaOpts: String = DPConfig.runJVMOpts)
extends SuiteRunner(testSourcePath, fileManager, updateCheck, failed, javaCmdPath, javacCmdPath, scalacExtraArgs, javaOpts) {

  if (!DPConfig.runTestsInParallel)
    sys.props("partest.threads") = "1"

  sys.props("partest.root") = "."

  // override to provide Dotty banner
  override def banner: String = {
    s"""|Welcome to Partest for Dotty! Partest version: ${Properties.versionNumberString}
        |Compiler under test: dotty.tools.dotc.Bench or dotty.tools.dotc.Main
        |Generated test sources: ${PathSettings.srcDir}${File.separator}
        |Test directories: ${DPConfig.testDirs.toList.mkString(", ")}
        |Debugging: failed tests have compiler output in test-kind.clog, run output in test-kind.log, class files in test-kind.obj
        |Parallel: ${DPConfig.runTestsInParallel}
        |Options: (use partest --help for usage information) ${consoleArgs}
    """.stripMargin
  }

  /** Some tests require a limitation of resources, tests which are compiled
    * with one or more of the flags in this list will be run with
    * `limitedThreads`. This is necessary because some test flags require a lot
    * of memory when running the compiler and may exhaust the available memory
    * when run in parallel with too many other tests.
    *
    * This number could be increased on the CI, but might fail locally if
    * scaled too extreme - override with:
    *
    * ```
    * -Ddotty.tests.limitedThreads=X
    * ```
    */
  def limitResourceFlags = List("-Ytest-pickler")
  private val limitedThreads = sys.props.get("dotty.tests.limitedThreads").getOrElse("2")

  override def runTestsForFiles(kindFiles: Array[File], kind: String): Array[TestState] = {
    val (limitResourceTests, parallelTests) =
      kindFiles partition { kindFile =>
        val flags = kindFile.changeExtension("flags").fileContents
        limitResourceFlags.exists(seqFlag => flags.contains(seqFlag))
      }

    val seqResults =
      if (!limitResourceTests.isEmpty) {
        val savedThreads = sys.props("partest.threads")
        sys.props("partest.threads") = {
          assert(
            savedThreads == null || limitedThreads.toInt <= savedThreads.toInt,
            """|Should not use more threads than the default, when the point
               |is to limit the amount of resources""".stripMargin
          )
          limitedThreads
        }

        NestUI.echo(s"## we will run ${limitResourceTests.length} tests using ${PartestDefaults.numThreads} thread(s) in parallel")
        val res = super.runTestsForFiles(limitResourceTests, kind)

        if (savedThreads != null)
          sys.props("partest.threads") = savedThreads
        else
          sys.props.remove("partest.threads")

        res
      } else Array[TestState]()

    val parResults =
      if (!parallelTests.isEmpty) {
        NestUI.echo(s"## we will run ${parallelTests.length} tests in parallel using ${PartestDefaults.numThreads} thread(s)")
        super.runTestsForFiles(parallelTests, kind)
      } else Array[TestState]()

    seqResults ++ parResults
  }

  // override for DPTestRunner and redirecting compilation output to test.clog
  override def runTest(testFile: File): TestState = {
    val runner = new DPTestRunner(testFile, this)

    val state =
      try {
        runner.run match {
          // Append compiler output to transcript if compilation failed,
          // printed with --verbose option
          case TestState.Fail(f, r@"compilation failed", transcript) =>
            TestState.Fail(f, r, transcript ++ runner.cLogFile.fileLines.dropWhile(_ == ""))
          case res => res
        }
      } catch {
        case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
      }
    reportTest(state)
    runner.cleanup()

    onFinishTest(testFile, state)
  }

  // override NestUI.reportTest because --show-diff doesn't work. The diff used
  // seems to add each line to transcript separately, whereas NestUI assumes
  // that the diff string was added as one entry in the transcript
  def reportTest(state: TestState) = {
    import NestUI._
    import NestUI.color._

    if (isTerse && state.isOk) {
      NestUI.reportTest(state)
    } else {
      echo(statusLine(state))
      if (!state.isOk && isDiffy) {
        val differ = bold(red("% ")) + "diff "
        state.transcript.dropWhile(s => !(s startsWith differ)) foreach (echo(_))
        // state.transcript find (_ startsWith differ) foreach (echo(_)) // original
      }
    }
  }
}

class DPTestRunner(testFile: File, suiteRunner: DPSuiteRunner) extends nest.Runner(testFile, suiteRunner) {
  val cLogFile = SFile(logFile).changeExtension("clog")

  // override to provide DottyCompiler
  override def newCompiler = new dotty.partest.DPDirectCompiler(this)

  // Adapted from nest.Runner#javac because:
  // - Our classpath handling is different and we need to pass extraClassPath
  //   to java to get the scala-library which is required for some java tests
  // - The compiler output should be redirected to cLogFile, like the output of
  //   dotty itself
  override def javac(files: List[File]): TestState = {
    import fileManager._
    import suiteRunner._
    import FileManager.joinPaths
    // compile using command-line javac compiler
    val args = Seq(
      suiteRunner.javacCmdPath, // FIXME: Dotty deviation just writing "javacCmdPath" doesn't work
      "-d",
      outDir.getAbsolutePath,
      "-classpath",
      joinPaths(outDir :: extraClasspath ++ testClassPath)
    ) ++ files.map(_.getAbsolutePath)

    pushTranscript(args mkString " ")

    val captured = StreamCapture(runCommand(args, cLogFile))
    if (captured.result) genPass() else {
      cLogFile appendAll captured.stderr
      cLogFile appendAll captured.stdout
      genFail("java compilation failed")
    }
  }

  // Overriden in order to recursively get all sources that should be handed to
  // the compiler. Otherwise only sources in the top dir is compiled - works
  // because the compiler is on the classpath.
  override def sources(file: File): List[File] =
    if (file.isDirectory)
      file.listFiles.toList.flatMap { f =>
        if (f.isDirectory) sources(f)
        else if (f.isJavaOrScala) List(f)
        else Nil
      }
    else List(file)

  // Enable me to "fix" the depth issue - remove once completed
  //override def compilationRounds(file: File): List[CompileRound] = {
  //  val srcs = sources(file) match {
  //    case Nil =>
  //      System.err.println {
  //        s"""|================================================================================
  //            |Warning! You attempted to compile sources from:
  //            |  $file
  //            |but partest was unable to find any sources - uncomment DPConsoleRunner#sources
  //            |================================================================================""".stripMargin
  //      }
  //      List(new File("./tests/pos/HelloWorld.scala")) // "just compile some crap" - Guillaume
  //    case xs =>
  //    xs
  //  }
  //  (groupedFiles(srcs) map mixedCompileGroup).flatten
  //}

  // FIXME: This is copy-pasted from nest.Runner where it is private
  // Remove this once https://github.com/scala/scala-partest/pull/61 is merged
  /** Runs command redirecting standard out and
   *  error out to output file.
   */
  def runCommand(args: Seq[String], outFile: File): Boolean = {
    import scala.sys.process.{ Process, ProcessLogger }
    //(Process(args) #> outFile !) == 0 or (Process(args) ! pl) == 0
    val pl = ProcessLogger(outFile)
    val nonzero = 17     // rounding down from 17.3
    def run: Int = {
      val p = Process(args) run pl
      try p.exitValue
      catch {
        case e: InterruptedException =>
          NestUI verbose s"Interrupted waiting for command to finish (${args mkString " "})"
          p.destroy
          nonzero
        case t: Throwable =>
          NestUI verbose s"Exception waiting for command to finish: $t (${args mkString " "})"
          p.destroy
          throw t
      }
      finally pl.close()
    }
    (pl buffer run) == 0
  }

  // override to provide default dotty flags from file in directory
  override def flagsForCompilation(sources: List[File]): List[String] = {
    val specificFlags = super.flagsForCompilation(sources)
    if (specificFlags.isEmpty) defaultFlags
    else specificFlags
  }

  val defaultFlags = {
    val defaultFile = parentFile.listFiles.toList.find(_.getName == "__defaultFlags.flags")
    defaultFile.map({ file =>
      SFile(file).safeSlurp.map({ content => words(content).filter(_.nonEmpty) }).getOrElse(Nil)
    }).getOrElse(Nil)
  }

  // override to add the check for nr of compilation errors if there's a
  // target.nerr file
  override def runNegTest() = runInContext {
    import scala.reflect.internal.FatalError

    sealed abstract class NegTestState
    // Don't get confused, the neg test passes when compilation fails for at
    // least one round (optionally checking the number of compiler errors and
    // compiler console output)
    case object CompFailed extends NegTestState
    // the neg test fails when all rounds return either of these:
    case class CompFailedButWrongNErr(expected: String, found: String) extends NegTestState
    case object CompFailedButWrongDiff extends NegTestState
    case object CompSucceeded extends NegTestState

    def nerrIsOk(reason: String) = {
      val nerrFinder = """compilation failed with (\d+) errors""".r
      reason match {
        case nerrFinder(found) =>
          SFile(FileOps(testFile) changeExtension "nerr").safeSlurp match {
            case Some(exp) if (exp != found) => CompFailedButWrongNErr(exp, found)
            case _ => CompFailed
          }
        case _ => CompFailed
      }
    }

    // we keep the partest semantics where only one round needs to fail
    // compilation, not all
    val compFailingRounds =
      compilationRounds(testFile)
      .map { round =>
        val ok = round.isOk
        setLastState(if (ok) genPass else genFail("compilation failed"))
        (round.result, ok)
      }
      .filter { case (_, ok) => !ok }

    val failureStates = compFailingRounds.map({ case (result, _) => result match {
      // or, OK, we'll let you crash the compiler with a FatalError if you supply a check file
      case Crash(_, t, _) if !checkFile.canRead || !t.isInstanceOf[FatalError] => CompSucceeded
      case Fail(_, reason, _) => if (diffIsOk) nerrIsOk(reason) else CompFailedButWrongDiff
      case _ => if (diffIsOk) CompFailed else CompFailedButWrongDiff
    }})

    if (failureStates.exists({ case CompFailed => true; case _ => false })) {
      true
    } else {
      val existsNerr = failureStates.exists({
        case CompFailedButWrongNErr(exp, found) =>
          nextTestActionFailing(s"wrong number of compilation errors, expected: $exp, found: $found")
          true
        case _ =>
          false
      })

      if (existsNerr) false
      else {
        val existsDiff = failureStates.exists({
          case CompFailedButWrongDiff =>
            nextTestActionFailing(s"output differs")
            true
          case _ =>
            false
        })
        if (existsDiff) false
        else nextTestActionFailing("expected compilation failure")
      }
    }
  }

  // override to change check file updating to original file, not generated
  override def diffIsOk: Boolean = {
    // always normalize the log first
    normalizeLog()
    val diff = currentDiff
    // if diff is not empty, is update needed?
    val updating: Option[Boolean] = (
      if (diff == "") None
      else Some(suiteRunner.updateCheck)
    )
    pushTranscript(s"diff $logFile $checkFile")
    nextTestAction(updating) {
      case Some(true) =>
        val origCheck = SFile(checkFile.changeExtension("checksrc").fileLines(1))
        NestUI.echo("Updating original checkfile " + origCheck)
        origCheck writeAll file2String(logFile)
        genUpdated()
      case Some(false) =>
        // Get a word-highlighted diff from git if we can find it
        val bestDiff = if (updating.isEmpty) "" else {
          if (checkFile.canRead)
            gitDiff(logFile, checkFile) getOrElse {
              s"diff $logFile $checkFile\n$diff"
            }
          else diff
        }
        pushTranscript(bestDiff)
        genFail("output differs")
      case None => genPass()  // redundant default case
    } getOrElse true
  }

  // override to add dotty and scala jars to classpath
  override def extraClasspath =
    suiteRunner.fileManager.asInstanceOf[DottyFileManager].extraJarList ::: super.extraClasspath


  // FIXME: Dotty deviation: error if return type is omitted:
  //   overriding method cleanup in class Runner of type ()Unit;
  //    method cleanup of type => Boolean | Unit has incompatible type

  // override to keep class files if failed and delete clog if ok
  override def cleanup: Unit = if (lastState.isOk) {
    logFile.delete
    cLogFile.delete
    Directory(outDir).deleteRecursively
  }
}
