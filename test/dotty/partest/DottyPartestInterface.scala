/* NOTE: adapted from 
 * https://github.com/scala/scala-partest-interface
 * TODO make the former configurable to avoid copy-pasting */

package dotty.partest

import sbt.testing.{ Fingerprint, TaskDef, EventHandler, Logger, Task, AnnotatedFingerprint }
import java.net.URLClassLoader
import java.io.File

object Framework {
  // as partest is not driven by test classes discovered by sbt, need to add this marker fingerprint to definedTests
  val fingerprint = new AnnotatedFingerprint { def isModule = true; def annotationName = "partest" }

  // TODO how can we export `fingerprint` so that a user can just add this to their build.sbt
  // definedTests in Test += new sbt.TestDefinition("partest", fingerprint, true, Array())
}
class Framework extends sbt.testing.Framework {
  def fingerprints: Array[Fingerprint] = Array(Framework.fingerprint)
  def name: String = "partest"

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): sbt.testing.Runner =
    new Runner(args, remoteArgs, testClassLoader)
}

/** Represents one run of a suite of tests.
 */
case class Runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader) extends sbt.testing.Runner {
  def tasks(taskDefs: Array[TaskDef]): Array[sbt.testing.Task] = taskDefs map (DottyPartestTask(_): sbt.testing.Task)

  def done(): String = ""
}

/** Run partest in this VM. Assumes we're running in a forked VM!
 *
 * TODO: make configurable
 */
case class DottyPartestTask(taskDef: TaskDef) extends Task {
  /** Executes this task, possibly returning to the client new tasks to execute. */
  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val forkedCp    = scala.util.Properties.javaClassPath
    val classLoader = new URLClassLoader(forkedCp.split(File.pathSeparator).map(new File(_).toURI.toURL))
    val runner      = new DottySBTSuiteRunner(Framework.fingerprint, eventHandler, loggers, new File(DottyPartestConfig.testRoot),
        classLoader, null, null, Array.empty[String])

    if (Runtime.getRuntime().maxMemory() / (1024*1024) < 800)
      loggers foreach (_.warn(s"Low heap size detected (~ ${Runtime.getRuntime().maxMemory() / (1024*1024)}M). Please add the following to your build.sbt: javaOptions in Test +=" + "\"-Xmx1G\""))

    try {
      val result = runner execute DottyPartestConfig.testDirs
      println("======================\n" + result)
    } catch {
      case ex: ClassNotFoundException =>
        loggers foreach { l => l.error("Please make sure partest is running in a forked VM by including the following line in build.sbt:\nfork in Test := true") }
        throw ex
    }

    Array()
  }

  /** A possibly zero-length array of string tags associated with this task. */
  def tags: Array[String] = Array()
}
