/* NOTE: Adapted from ScalaJSPartest.scala in
 * https://github.com/scala-js/scala-js/
 * TODO make partest configurable */

package dotty.partest

import scala.tools.partest._
import scala.tools.partest.nest._

import sbt.testing.{ EventHandler, Logger, Fingerprint }
import java.io.File
import java.net.URLClassLoader


class DottySBTSuiteRunner(
    partestFingerprint: Fingerprint, eventHandler: EventHandler,
    loggers: Array[Logger], testRoot: File,
    testClassLoader: URLClassLoader,
    javaCmd: File, javacCmd: File, scalacArgs: Array[String]
) extends SBTRunner(
    partestFingerprint, eventHandler, loggers, "", testClassLoader,
    javaCmd, javacCmd, scalacArgs
) {

  if (!DottyPartestConfig.runTestsInParallel)
    sys.props("partest.threads") = "1"

  sys.props("partest.root") = testRoot.getAbsolutePath()

  // override to provide Dotty banner
  override def banner: String = {
    s"""|Welcome to Partest for Dotty! Partest version: ${Properties.versionNumberString}
        |Compiler under test: dotty.tools.dotc.Bench or dotty.tools.dotc.Main
        |Test root: ${PathSettings.srcDir}${File.separator}
        |Test directories: ${DottyPartestConfig.testDirs.toList.mkString(", ")}
        |Parallel: ${DottyPartestConfig.runTestsInParallel}
    """.stripMargin
  }

  // override to provide DottyRunner
  override def runTest(testFile: File): TestState = {
    val runner = new DottyRunner(testFile, this)

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

class DottyRunner(testFile: File, suiteRunner: SuiteRunner) extends nest.Runner(testFile, suiteRunner) {
  // override to provide DottyCompiler
  override def newCompiler = new dotty.partest.DottyPartestDirectCompiler(this)

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

  // override to have several pos/neg directories, as long as prefix is pos/neg
  override def run(): TestState = {
    if (kind startsWith "pos") {
      runTestCommon(true)
      lastState
    } else if (kind startsWith "neg") {
      runNegTest()
      lastState
    } else {
      super.run()
    }
  }

  // override to add the check for nr of compilation errors if there's a
  // target.nerr file
  override def runNegTest() = runInContext {
    import TestState.{ Crash, Fail }
    import scala.reflect.internal.FatalError

    sealed abstract class State
    case class FoundFailed() extends State
    case class FailedWithWrongNErr(expected: String, found: String) extends State
    case class NotFailed() extends State

    def nerrIsOk(reason: String) = {
      import scala.util.matching.Regex
      val nerrFinder = """compilation failed with (\d\d+) errors""".r
      reason match {
        case nerrFinder(found) => 
          SFile(FileOps(testFile) changeExtension "nerr").safeSlurp match {
            case Some(exp) if (exp != found) => FailedWithWrongNErr(exp, found)
            case _ => FoundFailed
          }
        case _ => FoundFailed
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
      case Crash(_, t, _) if !checkFile.canRead || !t.isInstanceOf[FatalError] => NotFailed
      case Fail(_, reason, _) => if (diffIsOk) nerrIsOk(reason) else NotFailed
      case _ => if (diffIsOk) FoundFailed else NotFailed
    }})

    if (failureStates.exists({ case FoundFailed => true; case _ => false })) {
      true
    } else {
      val existsNerr = failureStates.exists({ 
        case FailedWithWrongNErr(exp, found) => nextTestActionFailing(s"wrong number of compilation errors, expected: $exp, found: $found"); true
        case _ => false
      })
      if (existsNerr) false else nextTestActionFailing("expected compilation failure")
    }
  }
}
