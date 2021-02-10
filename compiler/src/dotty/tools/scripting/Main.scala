package dotty.tools.scripting

import java.io.File
import java.nio.file.Path

/** Main entry point to the Scripting execution engine */
object Main:
  /** All arguments before -script <target_script> are compiler arguments.
      All arguments afterwards are script arguments.*/
  private def distinguishArgs(args: Array[String]): (Array[String], File, Array[String], Boolean) =
    val (leftArgs, rest) = args.splitAt(args.indexOf("-script"))
    if( rest.size < 2 ) then
      sys.error(s"missing: -script <scriptName>")

    val file = File(rest(1))
    val scriptArgs = rest.drop(2)
    var saveJar = false
    val compilerArgs = leftArgs.filter {
      case "-save" | "-savecompiled" =>
        saveJar = true
        false
      case _ =>
        true
    }
    (compilerArgs, file, scriptArgs, saveJar)
  end distinguishArgs

  def main(args: Array[String]): Unit =
    val (compilerArgs, scriptFile, scriptArgs, saveJar) = distinguishArgs(args)
    val driver = ScriptingDriver(compilerArgs, scriptFile, scriptArgs)
    try driver.compileAndRun { (outDir:Path, classpath:String, mainClass: String) =>
      if saveJar then
        // write a standalone jar to the script parent directory
        writeJarfile(outDir, scriptFile, scriptArgs, classpath, mainClass)
    }
    catch
      case ScriptingException(msg) =>
        println(s"Error: $msg")
        sys.exit(1)

      case e: java.lang.reflect.InvocationTargetException =>
        throw e.getCause

  private def writeJarfile(outDir: Path, scriptFile: File, scriptArgs:Array[String],
      classpath:String, mainClassName: String): Unit =

    val javaClasspath = sys.props("java.class.path")
    val runtimeClasspath = s"${classpath}$pathsep$javaClasspath"

    val jarTargetDir: Path = Option(scriptFile.toPath.getParent) match {
      case None => sys.error(s"no parent directory for script file [$scriptFile]")
      case Some(parent) => parent
    }

    def scriptBasename = scriptFile.getName.takeWhile(_!='.')
    val jarPath = s"$jarTargetDir/$scriptBasename.jar"

    val cpPaths = runtimeClasspath.split(pathsep).map {
      // protect relative paths from being converted to absolute
      case str if str.startsWith(".") && File(str).isDirectory => s"${str.withSlash}/"
      case str if str.startsWith(".") => str.withSlash
      case str => File(str).toURI.toURL.toString
    }

    import java.util.jar.Attributes.Name
    val cpString:String = cpPaths.distinct.mkString(" ")
    val manifestAttributes:Seq[(Name, String)] = Seq(
      (Name.MANIFEST_VERSION, "1.0.0"),
      (Name.MAIN_CLASS, mainClassName),
      (Name.CLASS_PATH, cpString),
    )
    import dotty.tools.io.{Jar, Directory}
    val jar = new Jar(jarPath)
    val writer = jar.jarWriter(manifestAttributes:_*)
    writer.writeAllFrom(Directory(outDir))
  end writeJarfile

  def pathsep = sys.props("path.separator")

  extension(pathstr:String) {
    def withSlash:String = pathstr.replace('\\', '/')
  }
