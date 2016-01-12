package test

import dotty.partest.DPConfig
import dotty.tools.dotc.{Main, Bench, Driver}
import dotty.tools.dotc.reporting.Reporter
import scala.collection.mutable.ListBuffer
import scala.reflect.io.{ Path, Directory, File => SFile }
import scala.tools.partest.nest.{ FileManager, NestUI }
import java.io.{ RandomAccessFile, File => JFile }

import org.junit.Test


/** This class has two modes: it can directly run compiler tests, or it can
  * generate the necessary file structure for partest in the directory
  * DPConfig.testRoot. Both modes are regular JUnit tests. Which mode is used
  * depends on the existence of the tests/locks/partest-ppid.lock file which is
  * created by sbt to trigger partest generation. Sbt will then run partest on
  * the generated sources.
  *
  * Through overriding the partestableXX methods, tests can always be run as
  * JUnit compiler tests. Run tests cannot be run by JUnit, only by partest.
  *
  * A test can either be a file or a directory. Partest will generate a
  * <test>-<kind>.log file with output of failed tests. Partest reads compiler
  * flags and the number of errors expected from a neg test from <test>.flags
  * and <test>.nerr files (also generated). The test is in a parent directory
  * that determines the kind of test:
  * - pos: checks that compilation succeeds
  * - neg: checks that compilation fails with the given number of errors
  * - run: compilation succeeds, partest: test run generates the output in
  *        <test>.check. Run tests always need to be:
  *        object Test { def main(args: Array[String]): Unit = ... }
  *        Classpath jars can be added to partestDeps in the sbt Build.scala.
  */
abstract class CompilerTest extends DottyTest {

  /** Override with output dir of test so it can be patched. Partest expects
    * classes to be in partest-generated/[kind]/[testname]-[kind].obj/ */
  val defaultOutputDir: String

  /** Override to filter out tests that should not be run by partest. */
  def partestableFile(prefix: String, fileName: String, extension: String, args: List[String], xerrors: Int) = true
  def partestableDir(prefix: String, dirName: String, args: List[String], xerrors: Int) = true
  def partestableList(testName: String, files: List[String], args: List[String], xerrors: Int) = true

  val generatePartestFiles = {
    /* Because we fork in test, the JVM in which this JUnit test runs has a
     * different pid from the one that started the partest. But the forked VM
     * receives the pid of the parent as system property. If the lock file
     * exists, the parent is requesting partest generation. This mechanism
     * allows one sbt instance to run test (JUnit only) and another partest.
     * We cannot run two instances of partest at the same time, because they're
     * writing to the same directories. The sbt lock file generation prevents
     * this.
     */
    val pid = System.getProperty("partestParentID")
    if (pid == null)
      false
    else
      new JFile("." + JFile.separator + "tests" + JFile.separator + "locks" + JFile.separator + s"partest-$pid.lock").exists
  }

  // Delete generated files from previous run and create new log
  val logFile = if (!generatePartestFiles) None else Some(CompilerTest.init)

  /** Always run with JUnit. */
  def compileLine(cmdLine: String, xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit = {
    if (generatePartestFiles)
      log("WARNING: compileLine will always run with JUnit, no partest files generated.")
    compileArgs(cmdLine.split("\n"), xerrors)
  }

  /** Compiles the given code file.
    *
    * @param prefix    the parent directory (including separator at the end)
    * @param fileName  the filename, by default without extension
    * @param args      arguments to the compiler
    * @param xerrors   if > 0, this test is a neg test with the expected number
    *                  of compiler errors. Otherwise, this is a pos test.
    * @param extension the file extension, .scala by default
    * @param defaultOptions more arguments to the compiler
    */
  def compileFile(prefix: String, fileName: String, args: List[String] = Nil, xerrors: Int = 0,
      extension: String = ".scala", runTest: Boolean = false)
      (implicit defaultOptions: List[String]): Unit = {
    if (!generatePartestFiles || !partestableFile(prefix, fileName, extension, args ++ defaultOptions, xerrors)) {
      if (runTest)
        log(s"WARNING: run tests can only be run by partest, JUnit just verifies compilation: $prefix$fileName$extension")
      compileArgs((s"$prefix$fileName$extension" :: args).toArray, xerrors)
    } else {
      val kind = testKind(prefix, xerrors, runTest)
      log(s"generating partest files for test file: $prefix$fileName$extension of kind $kind")

      val sourceFile = new JFile(prefix + fileName + extension)
      if (sourceFile.exists) {
        val firstDest = SFile(DPConfig.testRoot + JFile.separator + kind + JFile.separator + fileName + extension)
        computeDestAndCopyFiles(sourceFile, firstDest, kind, args ++ defaultOptions, xerrors.toString)
      } else {
        throw new java.io.FileNotFoundException(s"Unable to locate test file $prefix$fileName")
      }
    }
  }
  def runFile(prefix: String, fileName: String, args: List[String] = Nil, xerrors: Int = 0,
      extension: String = ".scala")(implicit defaultOptions: List[String]): Unit =
    compileFile(prefix, fileName, args, xerrors, extension, true)

  /** Compiles the code files in the given directory together. If args starts
    * with "-deep", all files in subdirectories (and so on) are included. */
  def compileDir(prefix: String, dirName: String, args: List[String] = Nil, xerrors: Int = 0, runTest: Boolean = false)
      (implicit defaultOptions: List[String]): Unit = {
    if (!generatePartestFiles || !partestableDir(prefix, dirName, args ++ defaultOptions, xerrors)) {
      if (runTest)
        log(s"WARNING: run tests can only be run by partest, JUnit just verifies compilation: $prefix$dirName")
      val dir = Directory(prefix + dirName)
      val (files, normArgs) = args match {
        case "-deep" :: args1 => (dir.deepFiles, args1)
        case _ => (dir.files, args)
      }
      val fileNames = files.toArray.map(_.toString).filter(name => (name endsWith ".scala") || (name endsWith ".java"))
      compileArgs(fileNames ++ normArgs, xerrors)
    } else {
      val (sourceDir, flags, deep) = args match {
        case "-deep" :: args1 => (flattenDir(prefix, dirName), args1 ++ defaultOptions, "deep")
        case _ => (new JFile(prefix + dirName), args ++ defaultOptions, "shallow")
      }
      val kind = testKind(prefix, xerrors, runTest)
      log(s"generating partest files for test directory ($deep): $prefix$dirName of kind $kind")

      if (sourceDir.exists) {
        val firstDest = Directory(DPConfig.testRoot + JFile.separator + kind + JFile.separator + dirName)
        computeDestAndCopyFiles(sourceDir, firstDest, kind, flags, xerrors.toString)
        if (deep == "deep")
          Directory(sourceDir).deleteRecursively
      } else {
        throw new java.io.FileNotFoundException(s"Unable to locate test dir $prefix$dirName")
      }
    }
  }
  def runDir(prefix: String, dirName: String, args: List[String] = Nil, xerrors: Int = 0)
      (implicit defaultOptions: List[String]): Unit =
    compileDir(prefix, dirName, args, xerrors, true)

  /** Compiles each source in the directory path separately by calling
    * compileFile resp. compileDir. */
  def compileFiles(path: String, args: List[String] = Nil, verbose: Boolean = true, runTest: Boolean = false)
      (implicit defaultOptions: List[String]): Unit = {
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.jfile.getName).filter(name => (name endsWith ".scala") || (name endsWith ".java"))
    for (name <- fileNames) {
      if (verbose) log(s"testing $path$name")
      compileFile(path, name, args, 0, "", runTest)
    }
    for (subdir <- dir.dirs) {
      if (verbose) log(s"testing $subdir")
      compileDir(path, subdir.jfile.getName, args, 0, runTest)
    }
  }
  def runFiles(path: String, args: List[String] = Nil, verbose: Boolean = true)
      (implicit defaultOptions: List[String]): Unit =
    compileFiles(path, args, verbose, true)

  /** Compiles the given list of code files. */
  def compileList(testName: String, files: List[String], args: List[String] = Nil, xerrors: Int = 0)
      (implicit defaultOptions: List[String]): Unit = {
    if (!generatePartestFiles || !partestableList(testName, files, args ++ defaultOptions, xerrors)) {
      compileArgs((files ++ args).toArray, xerrors)
    } else {
      val destDir = Directory(DPConfig.testRoot + JFile.separator + testName)
      files.foreach({ file =>
        val jfile = new JFile(file)
        recCopyFiles(jfile, destDir / jfile.getName)
      })
      compileDir(DPConfig.testRoot + JFile.separator, testName, args, xerrors)
      destDir.deleteRecursively
    }
  }

  // ========== HELPERS =============

  private def compileArgs(args: Array[String], xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit = {
    val allArgs = args ++ defaultOptions
    val processor = if (allArgs.exists(_.startsWith("#"))) Bench else Main
    val nerrors = processor.process(allArgs, ctx).errorCount
    assert(nerrors == xerrors, s"Wrong # of errors. Expected: $xerrors, found: $nerrors")
  }

  // In particular, don't copy flags from scalac tests
  private val extensionsToCopy = scala.collection.immutable.HashSet("scala", "java")

  /** Determines what kind of test to run. */
  private def testKind(prefixDir: String, xerrors: Int, runTest: Boolean) = {
    if (runTest) "run"
    else if (xerrors > 0) "neg"
    else if (prefixDir.endsWith("run" + JFile.separator)) {
      log("WARNING: test is being run as pos test despite being in a run directory. " +
        "Use runFile/runDir instead of compileFile/compileDir to do a run test")
      "pos"
    } else "pos"
  }

  /** The three possibilities: no generated sources exist yet, the same sources
    * exist already, different sources exist. */
  object Difference extends Enumeration {
    type Difference = Value
    val NotExists, ExistsSame, ExistsDifferent = Value
  }
  import Difference._

  /** The same source might be used for several partest test cases (e.g. with
    * different flags). Detects existing versions and computes the path to be
    * used for this version, e.g. testname_v1 for the first alternative. */
  private def computeDestAndCopyFiles(source: JFile, dest: Path, kind: String, oldFlags: List[String], nerr: String,
      nr: Int = 0, oldOutput: String = defaultOutputDir): Unit = {

    val partestOutput = dest.jfile.getParentFile + JFile.separator + dest.stripExtension + "-" + kind + ".obj"
    val flags = oldFlags.map(f => if (f == oldOutput) partestOutput else f)

    getExisting(dest).isDifferent(source, flags, nerr) match {
      case NotExists => copyFiles(source, dest, partestOutput, flags, nerr, kind)
      case ExistsSame => // nothing else to do
      case ExistsDifferent =>
        val nextDest = dest.parent / (dest match {
          case d: Directory => Directory(replaceVersion(d.name, nr))
          case f => SFile(replaceVersion(f.stripExtension, nr)).addExtension(f.extension)
        })
        computeDestAndCopyFiles(source, nextDest, kind, flags, nerr, nr + 1, partestOutput)
    }
  }

  /** Copies the test sources. Creates flags, nerr, check and output files. */
  private def copyFiles(sourceFile: Path, dest: Path, partestOutput: String, flags: List[String], nerr: String, kind: String) = {
    recCopyFiles(sourceFile, dest)

    new JFile(partestOutput).mkdirs

    if (flags.nonEmpty)
      dest.changeExtension("flags").createFile(true).writeAll(flags.mkString(" "))
    if (nerr != "0")
      dest.changeExtension("nerr").createFile(true).writeAll(nerr)
    sourceFile.changeExtension("check").ifFile({ check =>
      if (kind == "run") {
        FileManager.copyFile(check.jfile, dest.changeExtension("check").jfile)
        dest.changeExtension("checksrc").createFile(true).writeAll("check file generated from source:\n" + check.toString)
      } else {
        log(s"WARNING: ignoring $check for test kind $kind")
      }
    })

  }

  /** Recursively copy over source files and directories, excluding extensions
    * that aren't in extensionsToCopy. */
  private def recCopyFiles(sourceFile: Path, dest: Path): Unit = {
    processFileDir(sourceFile, { sf =>
      if (extensionsToCopy.contains(sf.extension)) {
        dest.parent.jfile.mkdirs
        dest.toFile.writeAll("/* ==========================================\n",
                             " * ========= AUTOMATICALLY GENERATED ========\n",
                             " * ========= DO NOT EDIT THIS FILE ==========\n",
                             " * ==========================================\n",
                             " * Original: " + sf.toString + " */\n\n",
                             sf.slurp())
      } else {
        log(s"WARNING: ignoring $sf")
      }
    }, { sdir =>
      dest.jfile.mkdirs
      sdir.list.foreach(path => recCopyFiles(path, dest / path.name))
    }, Some("DPCompilerTest.recCopyFiles: sourceFile not found: " + sourceFile))
  }

  /** Reads the existing files for the given test source if any. */
  private def getExisting(dest: Path): ExistingFiles = {
    val content: Option[Option[String]] = processFileDir(dest, f => f.safeSlurp, d => Some(""))
    if (content.isDefined && content.get.isDefined) {
      val flags = (dest changeExtension "flags").toFile.safeSlurp
      val nerr = (dest changeExtension "nerr").toFile.safeSlurp
      ExistingFiles(content.get, flags, nerr)
    } else ExistingFiles()
  }

  /** Encapsulates existing generated test files. */
  case class ExistingFiles(genSrc: Option[String] = None, flags: Option[String] = None, nerr: Option[String] = None) {
    def isDifferent(sourceFile: JFile, otherFlags: List[String], otherNerr: String): Difference = {
      if (!genSrc.isDefined) {
        NotExists
      } else {
        val source = processFileDir(sourceFile, { f => f.safeSlurp }, { d => Some("") },
            Some("DPCompilerTest sourceFile doesn't exist: " + sourceFile)).get
        if (source == genSrc) {
          nerr match {
            case Some(n) if (n != otherNerr) => ExistsDifferent
            case None if (otherNerr != "0") => ExistsDifferent
            case _ if (flags.map(_ == otherFlags.mkString(" ")).getOrElse(otherFlags.isEmpty)) => ExistsSame
            case _ => ExistsDifferent
          }
        } else ExistsDifferent
      }
    }
  }

  import scala.util.matching.Regex
  val nrFinder = """(.*_v)(\d+)""".r
  /** Changes the version number suffix in the name (without extension). */
  private def replaceVersion(name: String, nr: Int): String = {
    val nrString = nr.toString
    name match {
      case nrFinder(prefix, `nrString`) => prefix + (nr + 1)
      case _ =>
        assert(nr == 0, "DPCompilerTest couldn't create new version of files, match error")
        name + "_v1"
    }
  }

  /** Returns None if the given path doesn't exist, otherwise returns Some of
    * applying either processFile or processDir, depending on what the path
    * refers to in the file system. If failMsgOnNone is defined, this function
    * asserts that the file exists using the provided message. */
  private def processFileDir[T](input: Path, processFile: SFile => T, processDir: Directory => T, failMsgOnNone: Option[String] = None): Option[T] = {
    val res = input.ifFile(f => processFile(f)).orElse(input.ifDirectory(d => processDir(d)))
    (failMsgOnNone, res) match {
      case (Some(msg), None) => assert(false, msg); None
      case _ => res
    }
  }

  /** Creates a temporary directory and copies all (deep) files over, thus
    * flattening the directory structure. */
  private def flattenDir(prefix: String, dirName: String): JFile = {
    val destDir = Directory(DPConfig.testRoot + JFile.separator + "_temp")
    Directory(prefix + dirName).deepFiles.foreach(source => recCopyFiles(source, destDir / source.name))
    destDir.jfile
  }

  /** Write either to console (JUnit) or log file (partest). */
  private def log(msg: String) = logFile.map(_.appendAll(msg + "\n")).getOrElse(println(msg))
}

object CompilerTest extends App {

  /** Deletes generated partest sources from a previous run, recreates
    * directory and returns the freshly created log file. */
  lazy val init: SFile = {
    scala.reflect.io.Directory(DPConfig.testRoot).deleteRecursively
    new JFile(DPConfig.testRoot).mkdirs
    val log = DPConfig.genLog.createFile(true)
    println(s"CompilerTest is generating tests for partest, log: $log")
    log
  }

//  val dotcDir = "/Users/odersky/workspace/dotty/src/dotty/"

//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "CompilationUnit")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Compiler")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Driver")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Main")
//  new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Run")

//  new CompilerTest().compileDir(dotcDir + "tools/dotc")
 // new CompilerTest().compileFile(dotcDir + "tools/dotc/", "Run")
}
