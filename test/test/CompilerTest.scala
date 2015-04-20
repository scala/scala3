package test

import dotty.partest.DPConfig
import dotty.tools.dotc.{Main, Bench, Driver}
import dotty.tools.dotc.reporting.Reporter
import scala.collection.mutable.ListBuffer
import scala.reflect.io.{ Path, Directory, File => SFile }
import scala.tools.partest.nest.FileManager
import java.io.{ RandomAccessFile, File => JFile }

import org.junit.Test


/** This class has two modes: it can directly run compiler tests, or it can
  * generate the necessary file structure for partest in the directory
  * DPConfig.testRoot. Both modes are regular JUnit tests. Which mode is used
  * depends on the existence of the tests/runPartest.flag file which is created
  * by sbt to trigger partest generation. Sbt can then run partest on the
  * generated sources.
  *
  * Through overriding the partestableXX methods, tests can always be run as
  * JUnit compiler tests.
  *
  * A test can either be a file or a directory. The test is in a parent
  * directory that determines the kind of test:
  * - pos: checks that compilation succeeds
  * - neg: checks that compilation fails with the given number of errors
  * (- run: compilation succeeds and running generates the given output.)
  * For partest, compiler flags and the number of errors expected from a neg
  * test are read from test.flags and test.nerr files (also generated).
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
    val partestLockFile = "." + JFile.separator + "tests" + JFile.separator + "partest.lock"
    try {
      val partestLock = new RandomAccessFile(partestLockFile, "rw").getChannel.tryLock
      if (partestLock != null) { // file not locked by sbt -> don't generate partest
        partestLock.release
        false
      } else true
    } catch {
      // if sbt doesn't fork in Test, the tryLock request will throw instead of
      // returning null, because locks are per JVM, not per thread
      case ex: java.nio.channels.OverlappingFileLockException => true
    }
  }

  // Delete generated files from previous run
  if (generatePartestFiles)
    CompilerTest.init

  /** Always run with JUnit. */
  def compileLine(cmdLine: String, xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit =
    compileArgs(cmdLine.split("\n"), xerrors)

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
  def compileFile(prefix: String, fileName: String, args: List[String] = Nil, xerrors: Int = 0, extension: String = ".scala")
      (implicit defaultOptions: List[String]): Unit = {
    if (!generatePartestFiles || !partestableFile(prefix, fileName, extension, args ++ defaultOptions, xerrors)) {
      compileArgs((s"$prefix$fileName$extension" :: args).toArray, xerrors)
    } else {
      val kind = testKind(xerrors)
      println(s"generating partest files for test file: $prefix$fileName$extension of kind $kind")

      val sourceFile = new JFile(prefix + fileName + extension)
      if (sourceFile.exists) {
        val firstDest = SFile(DPConfig.testRoot + JFile.separator + kind + JFile.separator + fileName + extension)
        computeDestAndCopyFiles(sourceFile, firstDest, kind, args ++ defaultOptions, xerrors.toString)
      } else {
        throw new java.io.FileNotFoundException(s"Unable to locate test file $prefix$fileName")
      }
    }
  }

  /** Compiles the code files in the given directory together. If args starts
    * with "-deep", all files in subdirectories (and so on) are included. */
  def compileDir(prefix: String, dirName: String, args: List[String] = Nil, xerrors: Int = 0)(implicit defaultOptions: List[String]): Unit = {
    if (!generatePartestFiles || !partestableDir(prefix, dirName, args ++ defaultOptions, xerrors)) {
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
      val kind = testKind(xerrors)
      println(s"generating partest files for test directory ($deep): $prefix$dirName of kind $kind")

      if (sourceDir.exists) {
        val firstDest = Directory(DPConfig.testRoot + JFile.separator + kind + JFile.separator + dirName)
        computeDestAndCopyFiles(sourceDir, firstDest, kind, args ++ defaultOptions, xerrors.toString)
        if (deep == "deep") sourceDir.delete
      } else {
        throw new java.io.FileNotFoundException(s"Unable to locate test dir $prefix$dirName")
      }
    }
  }

  /** Compiles each source in the directory path separately by calling
    * compileFile resp. compileDir. */
  def compileFiles(path: String, args: List[String] = Nil, verbose: Boolean = true)
      (implicit defaultOptions: List[String]): Unit = {
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.jfile.getName).filter(name => (name endsWith ".scala") || (name endsWith ".java"))
    for (name <- fileNames) {
      if (verbose) println(s"testing $path$name")
      compileFile(path, name, args, 0, "")
    }
    for (subdir <- dir.dirs) {
      if (verbose) println(s"testing $subdir")
      compileDir(path, subdir.jfile.getName, args, 0)
    }
  }

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
  private def testKind(xerrors: Int) = if (xerrors > 0) "neg" else "pos"

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
      case NotExists => copyFiles(source, dest, partestOutput, flags, nerr)
      case ExistsSame => // nothing else to do
      case ExistsDifferent =>
        val nextDest = dest.parent / (dest match {
          case f: SFile => SFile(replaceVersion(f.stripExtension, nr)).addExtension(f.extension)
          case d: Directory => Directory(replaceVersion(d.name, nr))
        })
        computeDestAndCopyFiles(source, nextDest, kind, flags, nerr, nr + 1, partestOutput)
    }
  }
  
  /** Copies the test sources and creates flags, nerr and output files. */
  private def copyFiles(sourceFile: Path, dest: Path, partestOutput: String, flags: List[String], nerr: String) = {
    recCopyFiles(sourceFile, dest)

    new JFile(partestOutput).mkdirs

    if (flags.nonEmpty)
      dest.changeExtension("flags").createFile(true).writeAll(flags.mkString(" "))
    if (nerr != "0")
      dest.changeExtension("nerr").createFile(true).writeAll(nerr)
  }

  /** Recursively copy over files and directories, excluding extensions that
    * aren't in extensionsToCopy. */
  private def recCopyFiles(sourceFile: Path, dest: Path): Unit = {
    processFileDir(sourceFile, { sf =>
      if (extensionsToCopy.contains(sf.extension)) {
        dest.parent.jfile.mkdirs
        FileManager.copyFile(sourceFile.jfile, dest.jfile)
      } else {
        println(s"warning: ignoring $sf")
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
  
}

object CompilerTest extends App {

  /** Delete generated partest sources from a previous run. */
  lazy val init = {
    scala.reflect.io.Directory(DPConfig.testRoot).deleteRecursively
    new java.io.File(DPConfig.testRoot).mkdirs
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
