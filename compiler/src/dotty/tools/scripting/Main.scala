package dotty.tools.scripting

import scala.language.unsafeNulls

import java.io.File
import java.nio.file.{Path, Paths}
import dotty.tools.dotc.config.Properties.isWin

/** Main entry point to the Scripting execution engine */
object Main:
  /** All arguments before -script <target_script> are compiler arguments.
      All arguments afterwards are script arguments.*/
  private def distinguishArgs(args: Array[String]): (Array[String], File, Array[String], Boolean, Boolean) =
    val (leftArgs, rest) = args.splitAt(args.indexOf("-script"))
    assert(rest.size >= 2, s"internal error: rest == Array(${rest.mkString(",")})")

    val file = File(rest(1))
    // write script path to script.path property, so called script can see it
    sys.props("script.path") = file.toPath.toAbsolutePath.toString
    val scriptArgs = rest.drop(2)
    var saveJar = false
    var invokeFlag = true // by default, script main method is invoked
    val compilerArgs = leftArgs.filter {
      case "-save" | "-savecompiled" =>
        saveJar = true
        false
      case "-compile-only" =>
        invokeFlag = false // no call to script main method
        false
      case _ =>
        true
    }
    (compilerArgs, file, scriptArgs, saveJar, invokeFlag)
  end distinguishArgs

  def main(args: Array[String]): Unit =
    val (compilerArgs, scriptFile, scriptArgs, saveJar, invokeFlag) = distinguishArgs(args)
    val driver = ScriptingDriver(compilerArgs, scriptFile, scriptArgs)
    driver.compileAndRun { (outDir:Path, classpathEntries:Seq[Path], mainClass: String) =>
      // write expanded classpath to java.class.path property, so called script can see it
      sys.props("java.class.path") = classpathEntries.map(_.toString).mkString(pathsep)
      if saveJar then
        // write a standalone jar to the script parent directory
        writeJarfile(outDir, scriptFile, scriptArgs, classpathEntries, mainClass)
      invokeFlag
    } match
      case Some(ex) =>
        println(ex.getMessage)
        sys.exit(1)
      case _ =>

  private def writeJarfile(outDir: Path, scriptFile: File, scriptArgs:Array[String],
      classpathEntries:Seq[Path], mainClassName: String): Unit =

    val jarTargetDir: Path = Option(scriptFile.toPath.toAbsolutePath.getParent) match {
      case None => sys.error(s"no parent directory for script file [$scriptFile]")
      case Some(parent) => parent
    }

    def scriptBasename = scriptFile.getName.takeWhile(_!='.')
    val jarPath = s"$jarTargetDir/$scriptBasename.jar"

    val cpPaths = classpathEntries.map { _.toString.toUrl }

    import java.util.jar.Attributes.Name
    val cpString:String = cpPaths.distinct.mkString(" ")
    val manifestAttributes:Seq[(Name, String)] = Seq(
      (Name.MANIFEST_VERSION, "1.0"),
      (Name.MAIN_CLASS, mainClassName),
      (Name.CLASS_PATH, cpString),
    )
    import dotty.tools.io.{Jar, Directory}
    val jar = new Jar(jarPath)
    val writer = jar.jarWriter(manifestAttributes:_*)
    try
      writer.writeAllFrom(Directory(outDir))
    finally
      writer.close()
  end writeJarfile

  def pathsep = sys.props("path.separator")

  extension(path: String) {
    // Normalize path separator, convert relative path to absolute
    def norm: String =
      path.replace('\\', '/') match {
        case s if s.secondChar == ":" => s
        case s if s.startsWith("./") => s.drop(2)
        case s => s
      }

    // convert to absolute path relative to cwd.
    def absPath: String = norm match
      case str if str.isAbsolute => norm
      case _ => Paths.get(userDir, norm).toString.norm

    def toUrl: String = Paths.get(absPath).toUri.toURL.toString

    // Treat norm paths with a leading '/' as absolute.
    // Windows java.io.File#isAbsolute treats them as relative.
    def isAbsolute = path.norm.startsWith("/") || (isWin && path.secondChar == ":")
    def secondChar: String = path.take(2).drop(1).mkString("")
  }

  lazy val userDir = sys.props("user.dir").norm
