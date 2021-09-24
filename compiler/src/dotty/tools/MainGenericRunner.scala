package dotty.tools


import scala.annotation.tailrec
import scala.io.Source
import scala.util.{ Try, Success, Failure }
import java.net.URLClassLoader
import sys.process._
import java.io.File
import java.lang.Thread
import scala.annotation.internal.sharable
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.runner.ObjectRunner
import dotty.tools.dotc.config.Properties.envOrNone
import java.util.jar._
import java.util.jar.Attributes.Name
import dotty.tools.io.Jar
import dotty.tools.runner.ScalaClassLoader
import java.nio.file.{Files, Paths, Path}
import scala.collection.JavaConverters._
import dotty.tools.dotc.config.CommandLineParser

enum ExecuteMode:
  case Guess
  case Script
  case Repl
  case Run
  case PossibleRun

case class Settings(
  verbose: Boolean = false,
  classPath: List[String] = List.empty,
  executeMode: ExecuteMode = ExecuteMode.Guess,
  exitCode: Int = 0,
  javaArgs: List[String] = List.empty,
  scalaArgs: List[String] = List.empty,
  residualArgs: List[String] = List.empty,
  possibleEntryPaths: List[String] = List.empty,
  scriptArgs: List[String] = List.empty,
  targetScript: String = "",
  targetToRun: String = "",
  save: Boolean = false,
  modeShouldBePossibleRun: Boolean = false,
  modeShouldBeRun: Boolean = false,
  compiler: Boolean = false,
) {
  def withExecuteMode(em: ExecuteMode): Settings = this.executeMode match
    case ExecuteMode.Guess | ExecuteMode.PossibleRun =>
      this.copy(executeMode = em)
    case _ =>
      println(s"execute_mode==[$executeMode], attempted overwrite by [$em]")
      this.copy(exitCode = 1)
  end withExecuteMode

  def withScalaArgs(args: String*): Settings =
    this.copy(scalaArgs = scalaArgs.appendedAll(args.toList))

  def withJavaArgs(args: String*): Settings =
    this.copy(javaArgs = javaArgs.appendedAll(args.toList))

  def withResidualArgs(args: String*): Settings =
    this.copy(residualArgs = residualArgs.appendedAll(args.toList))

  def withPossibleEntryPaths(args: String*): Settings =
    this.copy(possibleEntryPaths = possibleEntryPaths.appendedAll(args.toList))

  def withScriptArgs(args: String*): Settings =
    this.copy(scriptArgs = scriptArgs.appendedAll(args.toList))

  def withTargetScript(file: String): Settings =
    Try(Source.fromFile(file)).toOption match
      case Some(_) => this.copy(targetScript = file)
      case None      =>
        println(s"not found $file")
        this.copy(exitCode = 2)
  end withTargetScript

  def withTargetToRun(targetToRun: String): Settings =
    this.copy(targetToRun = targetToRun)

  def withSave: Settings =
    this.copy(save = true)

  def withModeShouldBePossibleRun: Settings =
    this.copy(modeShouldBePossibleRun = true)

  def withModeShouldBeRun: Settings =
    this.copy(modeShouldBeRun = true)

  def withCompiler: Settings =
    this.copy(compiler = true)
}

object MainGenericRunner {

  val classpathSeparator = File.pathSeparator

  @sharable val javaOption = raw"""-J(.*)""".r
  @sharable val scalaOption = raw"""@.*""".r
  @sharable val colorOption = raw"""-color:.*""".r
  @tailrec
  def process(args: List[String], settings: Settings): Settings = args match
    case Nil =>
      settings
    case "-run" :: fqName :: tail =>
      process(tail, settings.withExecuteMode(ExecuteMode.Run).withTargetToRun(fqName))
    case ("-cp" | "-classpath" | "--class-path") :: cp :: tail =>
      val globdir = cp.replaceAll("[\\/][^\\/]*$", "") // slash/backslash agnostic
      val (tailargs, cpstr) = if globdir.nonEmpty && classpathSeparator != ";" || cp.contains(classpathSeparator) then
        (tail, cp)
      else
        // combine globbed classpath entries into a classpath
        val jarfiles = cp :: tail
        val cpfiles = jarfiles.takeWhile( f => f.startsWith(globdir) && ((f.toLowerCase.endsWith(".jar") || f.endsWith(".zip"))) )
        val tailargs = jarfiles.drop(cpfiles.size)
        (tailargs, cpfiles.mkString(classpathSeparator))
        
      process(tailargs, settings.copy(classPath = settings.classPath ++ cpstr.split(classpathSeparator).filter(_.nonEmpty)))

    case ("-version" | "--version") :: _ =>
      settings.copy(
        executeMode = ExecuteMode.Repl,
        residualArgs = List("-version")
      )
    case ("-v" | "-verbose" | "--verbose") :: tail =>
      process(
        tail,
        settings.copy(
          verbose = true,
          residualArgs = settings.residualArgs :+ "-verbose"
        )
      )
    case "-save" :: tail =>
      process(tail, settings.withSave)
    case "-with-compiler" :: tail =>
      process(tail, settings.withCompiler)
    case (o @ javaOption(striped)) :: tail =>
      process(tail, settings.withJavaArgs(striped).withScalaArgs(o))
    case (o @ scalaOption(_*)) :: tail =>
      val remainingArgs = (CommandLineParser.expandArg(o) ++ tail).toList
      process(remainingArgs, settings)
    case (o @ colorOption(_*)) :: tail =>
      process(tail, settings.withScalaArgs(o))
    case arg :: tail =>
      val line = Try(Source.fromFile(arg).getLines.toList).toOption.flatMap(_.headOption)
      if arg.endsWith(".scala") || arg.endsWith(".sc") || (line.nonEmpty && raw"#!.*scala".r.matches(line.get)) then
        settings
          .withExecuteMode(ExecuteMode.Script)
          .withTargetScript(arg)
          .withScriptArgs(tail*)
      else
        val newSettings = if arg.startsWith("-") then settings else settings.withPossibleEntryPaths(arg).withModeShouldBePossibleRun
        process(tail, newSettings.withResidualArgs(arg))

  def main(args: Array[String]): Unit =
    val scalaOpts = envOrNone("SCALA_OPTS").toArray.flatMap(_.split(" "))
    val allArgs = scalaOpts ++ args
    val settings = process(allArgs.toList, Settings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)

    def removeCompiler(cp: Array[String]) =
      if (!settings.compiler) then // Let's remove compiler from the classpath
        val compilerLibs = Seq("scala3-compiler", "scala3-interfaces", "tasty-core", "scala-asm", "scala3-staging", "scala3-tasty-inspector")
        cp.filterNot(c => compilerLibs.exists(c.contains))
      else
        cp

    def run(settings: Settings): Unit = settings.executeMode match
      case ExecuteMode.Repl =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)

      case ExecuteMode.PossibleRun =>
        val newClasspath = (settings.classPath :+ ".").flatMap(_.split(classpathSeparator).filter(_.nonEmpty)).map(File(_).toURI.toURL)
        import dotty.tools.runner.RichClassLoader._
        val newClassLoader = ScalaClassLoader.fromURLsParallelCapable(newClasspath)
        val targetToRun = settings.possibleEntryPaths.to(LazyList).find { entryPath =>
          newClassLoader.tryToLoadClass(entryPath).orElse {
            Option.when(Jar.isJarOrZip(dotty.tools.io.Path(entryPath)))(Jar(entryPath).mainClass).flatten
          }.isDefined
        }
        targetToRun match
          case Some(fqName) =>
            run(settings.withTargetToRun(fqName).withResidualArgs(settings.residualArgs.filter { _ != fqName }*).withExecuteMode(ExecuteMode.Run))
          case None =>
            run(settings.withExecuteMode(ExecuteMode.Repl))
      case ExecuteMode.Run =>
        val scalaClasspath = ClasspathFromClassloader(Thread.currentThread().getContextClassLoader).split(classpathSeparator)
        val newClasspath = (settings.classPath.flatMap(_.split(classpathSeparator).filter(_.nonEmpty)) ++ removeCompiler(scalaClasspath) :+ ".").map(File(_).toURI.toURL)
        val res = ObjectRunner.runAndCatch(newClasspath, settings.targetToRun, settings.residualArgs).flatMap {
          case ex: ClassNotFoundException if ex.getMessage == settings.targetToRun =>
            val file = settings.targetToRun
            Jar(file).mainClass match
              case Some(mc) =>
                ObjectRunner.runAndCatch(newClasspath :+ File(file).toURI.toURL, mc, settings.residualArgs)
              case None =>
                Some(IllegalArgumentException(s"No main class defined in manifest in jar: $file"))
          case ex => Some(ex)
        }
        errorFn("", res)
      case ExecuteMode.Script =>
        val targetScript = Paths.get(settings.targetScript).toFile
        val targetJar = settings.targetScript.replaceAll("[.][^\\/]*$", "")+".jar"
        val precompiledJar = Paths.get(targetJar).toFile
        def mainClass = Jar(targetJar).mainClass.getOrElse("") // throws exception if file not found
        val jarIsValid = precompiledJar.isFile && mainClass.nonEmpty && precompiledJar.lastModified >= targetScript.lastModified
        if jarIsValid then
          // precompiledJar exists, is newer than targetScript, and manifest defines a mainClass
          sys.props("script.path") = targetScript.toPath.toAbsolutePath.normalize.toString
          val scalaClasspath = ClasspathFromClassloader(Thread.currentThread().getContextClassLoader).split(classpathSeparator)
          val newClasspath = (settings.classPath.flatMap(_.split(classpathSeparator).filter(_.nonEmpty)) ++ removeCompiler(scalaClasspath) :+ ".").map(File(_).toURI.toURL)
          val mc = mainClass
          if mc.nonEmpty then
            ObjectRunner.runAndCatch(newClasspath :+ File(targetJar).toURI.toURL, mc, settings.scriptArgs)
          else
            Some(IllegalArgumentException(s"No main class defined in manifest in jar: $precompiledJar"))
        else
          val properArgs =
            List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
              ++ settings.residualArgs
              ++ (if settings.save then List("-save") else Nil)
              ++ settings.scalaArgs
              ++ List("-script", settings.targetScript)
              ++ settings.scriptArgs
          scripting.Main.main(properArgs.toArray)
      case ExecuteMode.Guess =>
        if settings.modeShouldBePossibleRun then
          run(settings.withExecuteMode(ExecuteMode.PossibleRun))
        else if settings.modeShouldBeRun then
          run(settings.withExecuteMode(ExecuteMode.Run))
        else
          run(settings.withExecuteMode(ExecuteMode.Repl))

    run(settings)


  def errorFn(str: String, e: Option[Throwable] = None, isFailure: Boolean = true): Boolean = {
    if (str.nonEmpty) Console.err.println(str)
    e.foreach(_.printStackTrace())
    !isFailure
  }
}
