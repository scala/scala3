package test

import dotty.tools.dotc.core.Contexts._
import dotty.partest.DPConfig
import dotty.tools.dotc.{Main, Bench, Driver}
import dotty.tools.dotc.interfaces.Diagnostic.ERROR
import dotty.tools.dotc.reporting._
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.io.PlainFile
import scala.collection.mutable.ListBuffer
import scala.reflect.io.{ Path, Directory, File => SFile, AbstractFile }
import scala.tools.partest.nest.{ FileManager, NestUI }
import scala.annotation.tailrec
import java.io.{ RandomAccessFile, File => JFile }


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
abstract class CompilerTest {

  /** Override with output dir of test so it can be patched. Partest expects
    * classes to be in partest-generated/[kind]/[testname]-[kind].obj/ */
  val defaultOutputDir: String

  /** Override to filter out tests that should not be run by partest. */
  def partestableFile(prefix: String, fileName: String, extension: String, args: List[String]) = true
  def partestableDir(prefix: String, dirName: String, args: List[String]) = true
  def partestableList(testName: String, files: List[String], args: List[String]) = true

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
  def compileLine(cmdLine: String)(implicit defaultOptions: List[String]): Unit = {
    if (generatePartestFiles)
      log("WARNING: compileLine will always run with JUnit, no partest files generated.")
    compileArgs(cmdLine.split("\n"), Nil)
  }

  /** Compiles the given code file.
    *
    * @param prefix    the parent directory (including separator at the end)
    * @param fileName  the filename, by default without extension
    * @param args      arguments to the compiler
    * @param extension the file extension, .scala by default
    * @param defaultOptions more arguments to the compiler
    */
  def compileFile(prefix: String, fileName: String, args: List[String] = Nil, extension: String = ".scala", runTest: Boolean = false)
      (implicit defaultOptions: List[String]): Unit = {
    val filePath = s"$prefix$fileName$extension"
    val expErrors = expectedErrors(filePath)
    if (!generatePartestFiles || !partestableFile(prefix, fileName, extension, args ++ defaultOptions)) {
      if (runTest)
        log(s"WARNING: run tests can only be run by partest, JUnit just verifies compilation: $prefix$fileName$extension")
      if (args.contains("-rewrite")) {
        val file = new PlainFile(filePath)
        val data = file.toByteArray
        // compile with rewrite
        compileArgs((filePath :: args).toArray, expErrors)
        // compile again, check that file now compiles without -language:Scala2
        val plainArgs = args.filter(arg => arg != "-rewrite" && arg != "-language:Scala2")
        compileFile(prefix, fileName, plainArgs, extension, runTest)
        // restore original test file
        val out = file.output
        out.write(data)
        out.close()
      }
      else compileArgs((filePath :: args).toArray, expErrors)
    } else {
      val kind = testKind(prefix, runTest)
      log(s"generating partest files for test file: $prefix$fileName$extension of kind $kind")

      val sourceFile = new JFile(prefix + fileName + extension)
      if (sourceFile.exists) {
        val firstDest = SFile(DPConfig.testRoot + JFile.separator + kind + JFile.separator + fileName + extension)
        val xerrors = expErrors.map(_.totalErrors).sum
        computeDestAndCopyFiles(sourceFile, firstDest, kind, args ++ defaultOptions, xerrors.toString)
      } else {
        throw new java.io.FileNotFoundException(s"Unable to locate test file $prefix$fileName")
      }
    }
  }
  def runFile(prefix: String, fileName: String, args: List[String] = Nil, extension: String = ".scala")
       (implicit defaultOptions: List[String]): Unit = {
    compileFile(prefix, fileName, args, extension, true)
  }

  /** Compiles the code files in the given directory together. If args starts
    * with "-deep", all files in subdirectories (and so on) are included. */
  def compileDir(prefix: String, dirName: String, args: List[String] = Nil, runTest: Boolean = false)
      (implicit defaultOptions: List[String]): Unit = {
    def computeFilePathsAndExpErrors = {
      val dir = Directory(prefix + dirName)
      val (files, normArgs) = args match {
        case "-deep" :: args1 => (dir.deepFiles, args1)
        case _ => (dir.files, args)
      }
      val filePaths = files.toArray.map(_.toString).filter(name => (name endsWith ".scala") || (name endsWith ".java"))
      val expErrors = expectedErrors(filePaths.toList)
      (filePaths, normArgs, expErrors)
    }
    if (!generatePartestFiles || !partestableDir(prefix, dirName, args ++ defaultOptions)) {
      if (runTest)
        log(s"WARNING: run tests can only be run by partest, JUnit just verifies compilation: $prefix$dirName")
      val (filePaths, normArgs, expErrors) = computeFilePathsAndExpErrors
      compileArgs(filePaths ++ normArgs, expErrors)
    } else {
      val (sourceDir, flags, deep) = args match {
        case "-deep" :: args1 => (flattenDir(prefix, dirName), args1 ++ defaultOptions, "deep")
        case _ => (new JFile(prefix + dirName), args ++ defaultOptions, "shallow")
      }
      val kind = testKind(prefix, runTest)
      log(s"generating partest files for test directory ($deep): $prefix$dirName of kind $kind")

      if (sourceDir.exists) {
        val firstDest = Directory(DPConfig.testRoot + JFile.separator + kind + JFile.separator + dirName)
        val xerrors = if (isNegTest(prefix)) {
          val (_, _, expErrors) = computeFilePathsAndExpErrors
          expErrors.map(_.totalErrors).sum
        } else 0
        computeDestAndCopyFiles(sourceDir, firstDest, kind, flags, xerrors.toString)
        if (deep == "deep")
          Directory(sourceDir).deleteRecursively
      } else {
        throw new java.io.FileNotFoundException(s"Unable to locate test dir $prefix$dirName")
      }
    }
  }
  def runDir(prefix: String, dirName: String, args: List[String] = Nil)
      (implicit defaultOptions: List[String]): Unit =
    compileDir(prefix, dirName, args, true)

  /** Compiles each source in the directory path separately by calling
    * compileFile resp. compileDir. */
  def compileFiles(path: String, args: List[String] = Nil, verbose: Boolean = true, runTest: Boolean = false,
                   compileSubDirs: Boolean = true)(implicit defaultOptions: List[String]): Unit = {
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.jfile.getName).filter(name => (name endsWith ".scala") || (name endsWith ".java"))
    for (name <- fileNames) {
      if (verbose) log(s"testing $path$name")
      compileFile(path, name, args, "", runTest)
    }
    if (compileSubDirs)
      for (subdir <- dir.dirs) {
        if (verbose) log(s"testing $subdir")
        compileDir(path, subdir.jfile.getName, args, runTest)
      }
  }
  def runFiles(path: String, args: List[String] = Nil, verbose: Boolean = true)
      (implicit defaultOptions: List[String]): Unit =
    compileFiles(path, args, verbose, true)

  /** Compiles the given list of code files. */
  def compileList(testName: String, files: List[String], args: List[String] = Nil)
      (implicit defaultOptions: List[String]): Unit = {
    if (!generatePartestFiles || !partestableList(testName, files, args ++ defaultOptions)) {
      val expErrors = expectedErrors(files)
      compileArgs((files ++ args).toArray, expErrors)
    } else {
      val destDir = Directory(DPConfig.testRoot + JFile.separator + testName)
      files.foreach({ file =>
        val jfile = new JFile(file)
        recCopyFiles(jfile, destDir / jfile.getName)
      })
      compileDir(DPConfig.testRoot + JFile.separator, testName, args)
      destDir.deleteRecursively
    }
  }

  def replFile(prefix: String, fileName: String): Unit = {
    val path = s"$prefix$fileName"
    val f = new PlainFile(path)
    val repl = new TestREPL(new String(f.toCharArray))
    repl.process(Array[String]())
    repl.check()
  }

  def replFiles(path: String): Unit = {
    val dir = Directory(path)
    val fileNames = dir.files.toArray.map(_.jfile.getName).filter(_ endsWith ".check")
    for (name <- fileNames) {
      log(s"testing $path$name")
      replFile(path, name)
    }
  }

  // ========== HELPERS =============

  private def expectedErrors(filePaths: List[String]): List[ErrorsInFile] = if (filePaths.exists(isNegTest(_))) filePaths.map(getErrors(_)) else Nil

  private def expectedErrors(filePath: String): List[ErrorsInFile] = expectedErrors(List(filePath))

  private def isNegTest(testPath: String) = testPath.contains(JFile.separator + "neg" + JFile.separator)

  private def compileArgs(args: Array[String], expectedErrorsPerFile: List[ErrorsInFile])
      (implicit defaultOptions: List[String]): Unit = {
    val allArgs = args ++ defaultOptions
    val processor = if (allArgs.exists(_.startsWith("#"))) Bench else Main
    val storeReporter = new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      private val consoleReporter = new ConsoleReporter()
      private var innerStoreReporter = new StoreReporter(consoleReporter)
      def doReport(d: Diagnostic)(implicit ctx: Context): Unit = {
        if (innerStoreReporter == null) {
          consoleReporter.report(d)
        } else {
          innerStoreReporter.report(d)
          if (d.level == ERROR) {
            innerStoreReporter.flush()
            innerStoreReporter = null
          }
        }
      }
    }
    val reporter = processor.process(allArgs, storeReporter)

    val nerrors = reporter.errorCount
    val xerrors = (expectedErrorsPerFile map {_.totalErrors}).sum
    def expectedErrorFiles =
      expectedErrorsPerFile.collect{
        case er if er.totalErrors > 0 => er.fileName
      }
    assert(nerrors == xerrors,
      s"""Wrong # of errors. Expected: $xerrors, found: $nerrors
         |Files with expected errors: $expectedErrorFiles
       """.stripMargin)
    // NEG TEST
    if (xerrors > 0) {
      val errorLines = reporter.allErrors.map(_.pos)
      // reporter didn't record as many errors as its errorCount says
      assert(errorLines.length == nerrors, s"Not enough errors recorded.")

      // Some compiler errors have an associated source position. Each error
      // needs to correspond to a "// error" marker on that line in the source
      // file and vice versa.
      // Other compiler errors don't have an associated source position. Their
      // number should correspond to the total count of "// nopos-error"
      // markers in all files
      val (errorsByFile, errorsWithoutPos) = errorLines.groupBy(_.source.file).toList.partition(_._1.toString != "<no source>")

      // check errors with source position
      val foundErrorsPerFile = errorsByFile.map({ case (fileName, errorList) =>
        val posErrorLinesToNr = errorList.groupBy(_.line).toList.map({ case (line, list) => (line, list.length) }).sortBy(_._1)
        ErrorsInFile(fileName.toString, 0, posErrorLinesToNr)
      })
      val expectedErrorsPerFileZeroed = expectedErrorsPerFile.map({
        case ErrorsInFile(fileName, _, posErrorLinesToNr) =>
          ErrorsInFile(fileName.toString, 0, posErrorLinesToNr)
      })
      checkErrorsWithPosition(expectedErrorsPerFileZeroed, foundErrorsPerFile)

      // check errors without source position
      val expectedNoPos = expectedErrorsPerFile.map(_.noposErrorNr).sum
      val foundNoPos = errorsWithoutPos.map(_._2.length).sum
      assert(foundNoPos == expectedNoPos,
        s"Wrong # of errors without source position. Expected (all files): $expectedNoPos, found (compiler): $foundNoPos")
    }
  }

  // ========== NEG TEST HELPERS =============

  /** Captures the number of nopos-errors in the given file and the number of
    * errors with a position, represented as a tuple of source line and number
    * of errors on that line. */
  case class ErrorsInFile(fileName: String, noposErrorNr: Int, posErrorLinesToNr: List[(Int, Int)]) {
    def totalErrors = noposErrorNr + posErrorLinesToNr.map(_._2).sum
  }

  /** Extracts the errors expected for the given neg test file. */
  def getErrors(fileName: String): ErrorsInFile = {
    val content = SFile(fileName).slurp
    val (line, rest) = content.span(_ != '\n')

    @tailrec
    def checkLine(line: String, rest: String, index: Int, noposAcc: Int, posAcc: List[(Int, Int)]): ErrorsInFile = {
      val posErrors = "// ?error".r.findAllIn(line).length
      val newPosAcc = if (posErrors > 0) (index, posErrors) :: posAcc else posAcc
      val newNoPosAcc = noposAcc + "// ?nopos-error".r.findAllIn(line).length
      val (newLine, newRest) = rest.span(_ != '\n')
      if (newRest.isEmpty)
        ErrorsInFile(fileName.toString, newNoPosAcc, newPosAcc.reverse)
      else
        checkLine(newLine, newRest.tail, index + 1, newNoPosAcc, newPosAcc) // skip leading '\n'
    }

    checkLine(line, rest.tail, 0, 0, Nil) // skip leading '\n'
  }

  /** Asserts that the expected and found number of errors correspond, and
    * otherwise throws an error with the filename, plus optionally a line
    * number if available. */
  def errorMsg(fileName: String, lineNumber: Option[Int], exp: Int, found: Int) = {
    val i = lineNumber.map({ i => ":" + (i + 1) }).getOrElse("")
    assert(found == exp, s"Wrong # of errors for $fileName$i. Expected (file): $exp, found (compiler): $found")
  }

  /** Compares the expected with the found errors and creates a nice error
    * message if they don't agree. */
  def checkErrorsWithPosition(expected: List[ErrorsInFile], found: List[ErrorsInFile]): Unit = {
      // create nice error messages
      expected.diff(found) match {
        case Nil => // nothing missing
        case ErrorsInFile(fileName, _, expectedLines) :: xs =>
          found.find(_.fileName == fileName) match {
            case None =>
              // expected some errors, but none found for this file
              errorMsg(fileName, None, expectedLines.map(_._2).sum, 0)
            case Some(ErrorsInFile(_,_,foundLines)) =>
              // found wrong number/location of markers for this file
              compareLines(fileName, expectedLines, foundLines)
          }
      }

      found.diff(expected) match {
        case Nil => // nothing missing
        case ErrorsInFile(fileName, _, foundLines) :: xs =>
          expected.find(_.fileName == fileName) match {
            case None =>
              // found some errors, but none expected for this file
              errorMsg(fileName, None, 0, foundLines.map(_._2).sum)
            case Some(ErrorsInFile(_,_,expectedLines)) =>
              // found wrong number/location of markers for this file
              compareLines(fileName, expectedLines, foundLines)
          }
      }
  }

  /** Gives an error message for one line where the expected number of errors and
    * the number of compiler errors differ. */
  def compareLines(fileName: String, expectedLines: List[(Int, Int)], foundLines: List[(Int, Int)]) = {
    expectedLines.foreach({ case (line, expNr) =>
      foundLines.find(_._1 == line) match {
        case Some((_, `expNr`)) => // this line is ok
        case Some((_, foundNr)) => errorMsg(fileName, Some(line), expNr, foundNr)
        case None               => errorMsg(fileName, Some(line), expNr, 0)
      }
    })
    foundLines.foreach({ case (line, foundNr) =>
      expectedLines.find(_._1 == line) match {
        case Some((_, `foundNr`)) => // this line is ok
        case Some((_, expNr))     => errorMsg(fileName, Some(line), expNr, foundNr)
        case None                 => errorMsg(fileName, Some(line), 0,     foundNr)
      }
    })
  }

  // ========== PARTEST HELPERS =============

  // In particular, don't copy flags from scalac tests
  private val extensionsToCopy = scala.collection.immutable.HashSet("scala", "java")

  /** Determines what kind of test to run. */
  private def testKind(prefixDir: String, runTest: Boolean) = {
    if (runTest) "run"
    else if (isNegTest(prefixDir)) "neg"
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
        dest.toFile.writeAll("/* !!!!! WARNING: DO NOT MODIFY. Original is at: $sf !!!!! */",
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
