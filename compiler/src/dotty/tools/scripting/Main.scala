package dotty.tools.scripting

import dotty.tools.dotc.config.Properties.isWin
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.nio.*

import java.util.jar.Attributes.Name

/** Main entry point to the Scripting execution engine */
object Main:
  /** All arguments before -script <target_script> are compiler arguments.
      All arguments afterwards are script arguments.*/
  private def distinguishArgs(args: Array[String]): (Array[String], File, Array[String], Boolean, Boolean) =
    val (leftArgs, rest) = args.splitAt(args.indexOf("-script"))
    assert(rest.size >= 2, s"internal error: rest == Array(${rest.mkString(",")})")

    val file = File.getOrCreateOnDisk(rest(1))
    // write script path to script.path property, so called script can see it
    sys.props("script.path") = file.path
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

  def process(args: Array[String]): Option[Throwable] =
    val (compilerArgs, scriptFile, scriptArgs, saveJar, invokeFlag) = distinguishArgs(args)
    val driver = ScriptingDriver(compilerArgs, scriptFile, scriptArgs)
    driver.compileAndRun { ctx ?=> (outDir: FileContainer, classpathEntries: Seq[FileContainer], mainClass: String) =>
      // write expanded classpath to java.class.path property, so the called script can see it
      sys.props("java.class.path") = classpathEntries.map(_.path).mkString(pathsep)
      if saveJar then
        // write a standalone jar to the script parent directory
        writeJarfile(outDir, scriptFile, scriptArgs, classpathEntries, mainClass)(using ctx)
      invokeFlag
    }

  def main(args: Array[String]): Unit =
   process(args).map {
      case ScriptingException(msg) => println(msg)
      case ex => ex.printStackTrace
   }.foreach(_ => System.exit(1))

  private def writeJarfile(outDir: FileContainer, scriptFile: File, scriptArgs:Array[String],
      classpathEntries: Seq[FileSystemEntry], mainClassName: String)(using Context): Unit =

    val jarTargetDir: FileContainer = scriptFile.parent

    val jarPath = s"$jarTargetDir/${scriptFile.nameWithoutExtension}.jar"

    val cpPaths = classpathEntries.flatMap(_.toURL)

    val cpString:String = cpPaths.distinct.mkString(" ")
    val manifestAttributes = Seq(
      (Name.MAIN_CLASS.toString, mainClassName),
      (Name.CLASS_PATH.toString, cpString),
    ).toMap
    val jarArchive = FileContainer.getFromFile(
      File.getOrCreateOnDisk(jarPath),
      FileContainer.DefaultJarVersion,
      FileContainer.DefaultCompressionLevel,
      manifestAttributes
    ).get
    try outDir.copyRecursivelyTo(jarArchive)
    finally jarArchive.close()
  end writeJarfile

  def pathsep: String = sys.props("path.separator").nn
