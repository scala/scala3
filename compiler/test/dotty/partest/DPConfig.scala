package dotty.partest

import scala.collection.JavaConversions._
import scala.reflect.io.Path
import java.io.File

import scala.tools.partest.PartestDefaults


/** Dotty Partest runs all tests in the provided testDirs located under
  * testRoot. There can be several directories with pos resp. neg tests, as
  * long as the prefix is pos/neg.
  *
  * Each testDir can also have a __defaultFlags.flags file, which provides
  * compiler flags and is used unless there's a specific flags file (e.g. for
  * test pos/A.scala, if there's a pos/A.flags file those flags are used,
  * otherwise pos/__defaultFlags.flags are used if the file exists).
  */
object DPConfig {
  /** Options used for _running_ the run tests.
   *  Note that this is different from the options used when _compiling_ tests,
   *  those are determined by the sbt configuration.
   */
  val runJVMOpts = s"-Xms64M -Xmx1024M ${PartestDefaults.javaOpts}"

  val testRoot = (Path("..") / Path("tests") / Path("partest-generated")).toString
  val genLog = Path(testRoot) / Path("gen.log")

  lazy val testDirs = {
    val root = new File(testRoot)
    val dirs = if (!root.exists) Array.empty[String] else root.listFiles.filter(_.isDirectory).map(_.getName)
    if (dirs.isEmpty)
      throw new Exception("Partest did not detect any generated sources")
    dirs
  }

  // Tests finish faster when running in parallel, but console output is
  // out of order and sometimes the compiler crashes
  val runTestsInParallel = true
}
