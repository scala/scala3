package dotty.tools.scripting

import java.io.File
import java.nio.file.Path
import java.net.URLClassLoader
import java.lang.reflect.{ Modifier, Method }

/** Main entry point to the Scripting execution engine */
object Main:
  /** All arguments before -script <target_script> are compiler arguments.
      All arguments afterwards are script arguments.*/
  private def distinguishArgs(args: Array[String]): (Array[String], File, Array[String], Boolean) =
    // NOTE: if -script <scriptName> not present, quit with error.
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
    try ScriptingDriver(compilerArgs, scriptFile, scriptArgs).compileAndRun { (outDir:Path, classpath:String) =>
      val classFiles = outDir.toFile.listFiles.toList match {
      case Nil => sys.error(s"no files below [$outDir]")
      case list => list
      }

      val (mainClassName, mainMethod) = detectMainClassAndMethod(outDir, classpath, scriptFile)

      if saveJar then
        // write a standalone jar to the script parent directory
        writeJarfile(outDir, scriptFile, scriptArgs, classpath, mainClassName)

      // invoke the compiled script main method
      mainMethod.invoke(null, scriptArgs)
    }
    catch
      case e:Exception =>
        e.printStackTrace
        println(s"Error: ${e.getMessage}")
        sys.exit(1)

      case e: java.lang.reflect.InvocationTargetException =>
        throw e.getCause

  private def writeJarfile(outDir: Path, scriptFile: File, scriptArgs:Array[String],
      classpath:String, mainClassName: String): Unit =
    val jarTargetDir: Path = Option(scriptFile.toPath.getParent) match {
      case None => sys.error(s"no parent directory for script file [$scriptFile]")
      case Some(parent) => parent
    }

    def scriptBasename = scriptFile.getName.takeWhile(_!='.')
    val jarPath = s"$jarTargetDir/$scriptBasename.jar"

    val cpPaths = classpath.split(pathsep).map {
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

  private def detectMainClassAndMethod(outDir: Path, classpath: String,
      scriptFile: File): (String, Method) =
    val outDirURL = outDir.toUri.toURL
    val classpathUrls = classpath.split(pathsep).map(File(_).toURI.toURL)
    val cl = URLClassLoader(classpathUrls :+ outDirURL)

    def collectMainMethods(target: File, path: String): List[(String, Method)] =
      val nameWithoutExtension = target.getName.takeWhile(_ != '.')
      val targetPath =
        if path.nonEmpty then s"${path}.${nameWithoutExtension}"
        else nameWithoutExtension

      if target.isDirectory then
        for
          packageMember <- target.listFiles.toList
          membersMainMethod <- collectMainMethods(packageMember, targetPath)
        yield membersMainMethod
      else if target.getName.endsWith(".class") then
        val cls = cl.loadClass(targetPath)
        try
          val method = cls.getMethod("main", classOf[Array[String]])
          if Modifier.isStatic(method.getModifiers) then List((cls.getName, method)) else Nil
        catch
          case _: java.lang.NoSuchMethodException => Nil
      else Nil
    end collectMainMethods

    val candidates = for
      file <- outDir.toFile.listFiles.toList
      method <- collectMainMethods(file, "")
    yield method

    candidates match
      case Nil =>
        throw ScriptingException(s"No main methods detected in script ${scriptFile}")
      case _ :: _ :: _ =>
        throw ScriptingException("A script must contain only one main method. " +
          s"Detected the following main methods:\n${candidates.mkString("\n")}")
      case m :: Nil => m
    end match
  end detectMainClassAndMethod

  def pathsep = sys.props("path.separator")

  extension(pathstr:String) {
    def withSlash:String = pathstr.replace('\\', '/')
  }
