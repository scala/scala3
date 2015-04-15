/* NOTE: Adapted from ScalaJSPartest.scala in
 * https://github.com/scala-js/scala-js/
 * TODO make partest configurable */

package dotty.partest

import scala.tools.partest._
import scala.tools.partest.nest._
import java.io.File
import java.net.URLClassLoader

/** Runs dotty partest from the Console, discovering test sources in
  * DPConfig.testRoot that have been generated automatically by
  * DPPrepJUnitRunner. Use `sbt test` to run.
  */
object DPConsoleRunner {
  def main(args: Array[String]): Unit = {
    new DPConsoleRunner(args mkString (" ")).runPartest
  }
}

// console runner has a suite runner which creates a test runner for each test
class DPConsoleRunner(args: String) extends ConsoleRunner(args) {
  override val suiteRunner = new DPSuiteRunner (
    testSourcePath = optSourcePath getOrElse DPConfig.testRoot,
    fileManager = null, // new FileManager(ClassPath split PathResolver.Environment.javaUserClassPath map (Path(_))), // the script sets up our classpath for us via ant
    updateCheck = optUpdateCheck,
    failed = optFailed)

  override def run = {}
  def runPartest = super.run
}

class DPSuiteRunner(testSourcePath: String, // relative path, like "files", or "pending"
  fileManager: FileManager,
  updateCheck: Boolean,
  failed: Boolean,
  javaCmdPath: String = PartestDefaults.javaCmd,
  javacCmdPath: String = PartestDefaults.javacCmd,
  scalacExtraArgs: Seq[String] = Seq.empty) 
extends SuiteRunner(testSourcePath, fileManager, updateCheck, failed, javaCmdPath, javacCmdPath, scalacExtraArgs) {

  if (!DPConfig.runTestsInParallel)
    sys.props("partest.threads") = "1"

  sys.props("partest.root") = "."

  // override to provide Dotty banner
  override def banner: String = {
    s"""|Welcome to Partest for Dotty! Partest version: ${Properties.versionNumberString}
        |Compiler under test: dotty.tools.dotc.Bench or dotty.tools.dotc.Main
        |Test root: ${PathSettings.srcDir}${File.separator}
        |Test directories: ${DPConfig.testDirs.toList.mkString(", ")}
        |Parallel: ${DPConfig.runTestsInParallel}
    """.stripMargin
  }

  // override to provide DPTestRunner
  override def runTest(testFile: File): TestState = {
    val runner = new DPTestRunner(testFile, this)

    // when option "--failed" is provided execute test only if log
    // is present (which means it failed before)
    val state =
      if (failed && !runner.logFile.canRead)
        runner.genPass()
      else {
        val (state, _) =
          try timed(runner.run())
          catch {
            case t: Throwable => throw new RuntimeException(s"Error running $testFile", t)
          }
        NestUI.reportTest(state)
        runner.cleanup()
        state
      }
    onFinishTest(testFile, state)
  }

  // override val fileManager = new DottyFileManager(testClassLoader)
  // sbt package generates a dotty compiler jar, currently
  // ".../git/dotty/target/scala-2.11/dotty_2.11-0.1-SNAPSHOT.jar"
  // but it doesn't seem to be used anywhere
}

class DPTestRunner(testFile: File, suiteRunner: SuiteRunner) extends nest.Runner(testFile, suiteRunner) {
  // override to provide DottyCompiler
  override def newCompiler = new dotty.partest.DPDirectCompiler(this)

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
    import TestState.{ Crash, Fail }
    import scala.reflect.internal.FatalError

    sealed abstract class NegTestState
    // Don't get confused, the neg test passes when compilation fails for at
    // least one round (optionally checking the number of compiler errors and
    // compiler console output)
    case class CompFailed() extends NegTestState
    // the neg test fails when all rounds return either of these:
    case class CompFailedButWrongNErr(expected: String, found: String) extends NegTestState
    case class CompFailedButWrongDiff() extends NegTestState
    case class CompSucceeded() extends NegTestState

    def nerrIsOk(reason: String) = {
      import scala.util.matching.Regex
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
    val compFailingRounds = compilationRounds(testFile).map({round => 
      val ok = round.isOk
      setLastState(if (ok) genPass else genFail("compilation failed"))
      (round.result, ok)
    }).filter({ case (_, ok) => !ok })

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
        case CompFailedButWrongNErr(exp, found) => nextTestActionFailing(s"wrong number of compilation errors, expected: $exp, found: $found"); true
        case _ => false
      })
      if (existsNerr) {
        false 
      } else {
        val existsDiff = failureStates.exists({ 
          case CompFailedButWrongDiff() => nextTestActionFailing(s"output differs"); true
          case _ => false
        })
        if (existsDiff) {
          false
        } else {
          nextTestActionFailing("expected compilation failure")
        }
      }
    }
  }

  // override because Dotty currently doesn't handle separate compilation well,
  // so we ignore groups (tests suffixed with _1 and _2)
  override def groupedFiles(sources: List[File]): List[List[File]] = {
    val grouped = sources groupBy (_.group)
    val flatGroup = List(grouped.keys.toList.sorted.map({ k => grouped(k) sortBy (_.getName) }).flatten)
    try { // try/catch because of bug in partest
      if (flatGroup != super.groupedFiles(sources))
        NestUI.echoWarning("Warning: Overriding compilation groups for tests: " + sources)
    } catch {
      case e: java.lang.UnsupportedOperationException => NestUI.echoWarning("Warning: Overriding compilation groups for tests: " + sources)
    }
    flatGroup
  }

  // override to avoid separate compilation of scala and java sources
  override def mixedCompileGroup(allFiles: List[File]): List[CompileRound] = List(OnlyDotty(allFiles))
  case class OnlyDotty(fs: List[File]) extends CompileRound {
    def description = s"dotc $fsString"
    lazy val result = { pushTranscript(description) ; attemptCompile(fs) }
  }
}
