package dotty.tools.dotc

import core.Contexts._
import interfaces.Diagnostic.ERROR
import reporting._
import diagnostic.MessageContainer
import util.SourcePosition
import config.CompilerCommand
import dotty.tools.io.PlainFile
import scala.collection.mutable.ListBuffer
import dotty.tools.io.{ Path, Directory, File => SFile, AbstractFile }
import scala.annotation.tailrec
import java.io.{ RandomAccessFile, File => JFile }


/** Legacy compiler tests that run single threaded */
abstract class CompilerTest {

  /** Override with output dir of test so it can be patched. Partest expects
    * classes to be in partest-generated/[kind]/[testname]-[kind].obj/ */
  val defaultOutputDir: String

  /** Override to filter out tests that should not be run by partest. */
  def partestableFile(prefix: String, fileName: String, extension: String, args: List[String]) = true
  def partestableDir(prefix: String, dirName: String, args: List[String]) = true
  def partestableList(testName: String, files: List[String], args: List[String]) = true

  /** Always run with JUnit. */
  def compileLine(cmdLine: String)(implicit defaultOptions: List[String]): Unit =
    compileArgs(cmdLine.split("\n"), Nil)

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
  }
  def runFile(prefix: String, fileName: String, args: List[String] = Nil, extension: String = ".scala")
       (implicit defaultOptions: List[String]): Unit = {
    compileFile(prefix, fileName, args, extension, true)
  }

  def findJarFromRuntime(partialName: String): String = {
    val urls = ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile.toString)
    urls.find(_.contains(partialName)).getOrElse {
      throw new java.io.FileNotFoundException(
        s"""Unable to locate $partialName on classpath:\n${urls.toList.mkString("\n")}"""
      )
    }
  }

  private def compileWithJavac(
    fs: Array[String],
    args: Array[String]
  )(implicit defaultOptions: List[String]): Boolean = {
    val scalaLib = dotty.Jars.scalaLibrary
    val fullArgs = Array(
      "javac",
      "-classpath",
      s".:$scalaLib"
    ) ++ args ++ defaultOptions.dropWhile("-d" != _).take(2) ++ fs

    Runtime.getRuntime.exec(fullArgs).waitFor() == 0
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
      val (filePaths, javaFilePaths) = files
        .toArray.map(_.toString)
        .foldLeft((Array.empty[String], Array.empty[String])) { case (acc @ (fp, jfp), name) =>
          if (name endsWith ".scala") (name +: fp, jfp)
          else if (name endsWith ".java") (fp, name +: jfp)
          else (fp, jfp)
        }
      val expErrors = expectedErrors(filePaths.toList)
      (filePaths, javaFilePaths, normArgs, expErrors)
    }
    if (runTest)
      log(s"WARNING: run tests can only be run by partest, JUnit just verifies compilation: $prefix$dirName")
    val (filePaths, javaFilePaths, normArgs, expErrors) = computeFilePathsAndExpErrors
    compileWithJavac(javaFilePaths, Array.empty) // javac needs to run first on dotty-library
    compileArgs(javaFilePaths ++ filePaths ++ normArgs, expErrors)
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
    val expErrors = expectedErrors(files)
    compileArgs((files ++ args).toArray, expErrors)
  }

  // ========== HELPERS =============

  private def expectedErrors(filePaths: List[String]): List[ErrorsInFile] = if (filePaths.exists(isNegTest(_))) filePaths.map(getErrors(_)) else Nil

  private def expectedErrors(filePath: String): List[ErrorsInFile] = expectedErrors(List(filePath))

  private def isNegTest(testPath: String) = testPath.contains("/neg/")

  private def compileArgs(args: Array[String], expectedErrorsPerFile: List[ErrorsInFile])
      (implicit defaultOptions: List[String]): Unit = {
    val allArgs = args ++ defaultOptions
    val verbose = allArgs.contains("-verbose")
    //println(s"""all args: ${allArgs.mkString("\n")}""")
    val processor = if (allArgs.exists(_.startsWith("#"))) Bench else Main
    val storeReporter = new Reporter with UniqueMessagePositions with HideNonSensicalMessages {
      private val consoleReporter = new ConsoleReporter()
      private val innerStoreReporter = new StoreReporter(consoleReporter)
      def doReport(m: MessageContainer)(implicit ctx: Context): Unit = {
        if (m.level == ERROR || verbose) {
          innerStoreReporter.flush()
          consoleReporter.doReport(m)
        }
        else if (errorCount > 0) consoleReporter.doReport(m)
        else innerStoreReporter.doReport(m)
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
         |errors:
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
    val content = SFile(fileName).slurp()
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
    expectedLines foreach{
      case (line, expNr) =>
        foundLines.find(_._1 == line) match {
          case Some((_, `expNr`)) => // this line is ok
          case Some((_, foundNr)) => errorMsg(fileName, Some(line), expNr, foundNr)
          case None               =>
            println(s"expected lines = $expectedLines%, %")
            println(s"found lines = $foundLines%, %")
            errorMsg(fileName, Some(line), expNr, 0)
        }
    }
    foundLines foreach {
      case (line, foundNr) =>
        expectedLines.find(_._1 == line) match {
          case Some((_, `foundNr`)) => // this line is ok
          case Some((_, expNr))     => errorMsg(fileName, Some(line), expNr, foundNr)
          case None                 => errorMsg(fileName, Some(line), 0,     foundNr)
        }
    }
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

  /** Recursively copy over source files and directories, excluding extensions
    * that aren't in extensionsToCopy. */
  private def recCopyFiles(sourceFile: Path, dest: Path): Unit = {

    @tailrec def copyfile(file: SFile, bytewise: Boolean): Unit = {
      if (bytewise) {
        val in = file.inputStream()
        val out = SFile(dest).outputStream()
        val buffer = new Array[Byte](1024)
        @tailrec def loop(available: Int):Unit = {
          if (available < 0) {()}
          else {
            out.write(buffer, 0, available)
            val read = in.read(buffer)
            loop(read)
          }
        }
        loop(0)
        in.close()
        out.close()
      } else {
        try {
          SFile(dest)(scala.io.Codec.UTF8).writeAll((s"/* !!!!! WARNING: DO NOT MODIFY. Original is at: $file !!!!! */").replace("\\", "/"), file.slurp("UTF-8"))
        } catch {
          case unmappable: java.nio.charset.MalformedInputException =>
            copyfile(file, true) //there are bytes that can't be mapped with UTF-8. Bail and just do a straight byte-wise copy without the warning header.
        }
      }
    }

    processFileDir(sourceFile, { sf =>
      if (extensionsToCopy.contains(sf.extension)) {
        dest.parent.jfile.mkdirs
        copyfile(sf, false)
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
    val content: Option[Option[String]] = processFileDir(dest, f => try Some(f.slurp("UTF8")) catch {case io: java.io.IOException => Some(io.toString())}, d => Some(""))
    if (content.isDefined && content.get.isDefined) {
      val flags = (dest changeExtension "flags").toFile.safeSlurp()
      val nerr = (dest changeExtension "nerr").toFile.safeSlurp()
      ExistingFiles(content.get, flags, nerr)
    } else ExistingFiles()
  }

  /** Encapsulates existing generated test files. */
  case class ExistingFiles(genSrc: Option[String] = None, flags: Option[String] = None, nerr: Option[String] = None) {
    def isDifferent(sourceFile: JFile, otherFlags: List[String], otherNerr: String): Difference = {
      if (!genSrc.isDefined) {
        NotExists
      } else {
        val source = processFileDir(sourceFile, { f => try Some(f.slurp("UTF8")) catch {case _: java.io.IOException => None} }, { d => Some("") },
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
  private def replaceVersion(name: String, nr: Int): Option[String] = {
    val nrString = nr.toString
    name match {
      case nrFinder(prefix, `nrString`) => Some(prefix + (nr + 1))
      case _ if nr != 0 => None
      case _ => Some(name + "_v1")
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

  /** Write either to console */
  private def log(msg: String) = println(msg)
}
