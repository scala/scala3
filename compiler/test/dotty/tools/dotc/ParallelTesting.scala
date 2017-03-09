package dotty
package tools
package dotc

import java.io.{ File => JFile }
import scala.io.Source

import core.Contexts._
import reporting.{ Reporter, UniqueMessagePositions, HideNonSensicalMessages, MessageRendering }
import reporting.diagnostic.MessageContainer
import interfaces.Diagnostic.ERROR
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.{ Files, Path, Paths }

trait ParallelTesting {

  private val driver = new Driver {
    override def newCompiler(implicit ctx: Context) = new Compiler
  }

  private class DaftReporter(suppress: Boolean)
  extends Reporter with UniqueMessagePositions with HideNonSensicalMessages
  with MessageRendering {
    private var _errors: List[MessageContainer] = Nil
    def errors = _errors

    override def doReport(m: MessageContainer)(implicit ctx: Context) = {
      if (!suppress && m.level == ERROR) {
        _errors = m :: _errors
        System.err.println(messageAndPos(m.contained, m.pos, diagnosticLevel(m)))
      }
    }
  }

  private def compile(files: Array[JFile], flags: Array[String]): (Array[JFile], List[MessageContainer]) = {

    def findJarFromRuntime(partialName: String) = {
      val urls = ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile.toString)
      urls.find(_.contains(partialName)).getOrElse {
        throw new java.io.FileNotFoundException(
          s"""Unable to locate $partialName on classpath:\n${urls.toList.mkString("\n")}"""
        )
      }
    }

    def compileWithJavac(fs: Array[String]) = if (fs.nonEmpty) {
      val scalaLib = findJarFromRuntime("scala-library")
      val fullArgs = Array(
        "javac",
        "-classpath",
        s".:$scalaLib"
      ) ++ flags.takeRight(2) ++ fs

      assert(Runtime.getRuntime.exec(fullArgs).waitFor() == 0, s"java compilation failed for ${fs.mkString(", ")}")
    }

    compileWithJavac(files.filter(_.getName.endsWith(".java")).map(_.getAbsolutePath))

    val reporter = new DaftReporter(suppress = false)
    driver.process(flags ++ files.map(_.getAbsolutePath), reporter = reporter)
    files -> reporter.errors
  }

  def compileFilesInDir(f: String, flags: Array[String])(implicit outDir: String): Unit = {
    val dir = new JFile(f)
    require(f.contains("/tests"), "only allowed to run integration tests from `tests` dir using this method")
    require(dir.isDirectory && dir.exists, "passed non-directory to `compileFilesInDir`")
    require(outDir.last == '/', "please specify an `outDir` with a trailing slash")

    def toCompilerDirFromDir(d: JFile): JFile = {
      val targetDir = new JFile(outDir + s"${dir.getName}/${d.getName}")
      // create if not exists
      targetDir.mkdirs()
      d.listFiles.foreach(copyToDir(targetDir, _))
      targetDir
    }
    def toCompilerDirFromFile(file: JFile): JFile = {
      val uniqueSubdir = file.getName.substring(0, file.getName.lastIndexOf('.'))
      val targetDir = new JFile(outDir + s"${dir.getName}/$uniqueSubdir")
      // create if not exists
      targetDir.mkdirs()
      // copy file to dir:
      copyToDir(targetDir, file)
      targetDir
    }
    def copyToDir(dir: JFile, file: JFile): Unit = {
      val target = Paths.get(dir.getAbsolutePath, file.getName)
      Files.copy(file.toPath, target, REPLACE_EXISTING).toFile
    }

    val (dirs, files) =
      dir.listFiles.foldLeft((List.empty[JFile], List.empty[JFile])) { case ((dirs, files), f) =>
        if (f.isDirectory) (f :: dirs, files)
        else (dirs, f :: files)
      }

    // Directories in which to compile all containing files with `flags`:
    val dirsToCompile = files.map(toCompilerDirFromFile) ++ dirs.map(toCompilerDirFromDir)

    // Progress bar setup
    val numberOfTargets = dirsToCompile.length
    var targetsCompiled = 0
    val start = System.currentTimeMillis
    var errors = 0

    dirsToCompile.map { dir =>
      val sourceFiles = dir.listFiles.filter(f => f.getName.endsWith(".scala") || f.getName.endsWith(".java"))
      targetsCompiled += 1
      val timestamp = (System.currentTimeMillis - start) / 1000
      val progress = (targetsCompiled.toDouble / numberOfTargets * 40).toInt
      print(
        s"Compiling tests in $f [" +
        ("=" * (math.max(progress - 1, 0))) +
        (if (progress > 0) ">" else "") +
        (" " * (39 - progress)) +
        s"] $targetsCompiled/$numberOfTargets, ${timestamp}s, errors: $errors\r"
      )
      val (_, newErrors ) = compile(sourceFiles, flags ++ Array("-d", dir.getAbsolutePath))
      errors += newErrors.length
    }
    println(s"Compiled tests in $f [========================================] $targetsCompiled/$numberOfTargets, ${(System.currentTimeMillis - start) / 1000}s, errors: $errors  ")

  }
}
