package dotty.tools

import scala.language.unsafeNulls

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import java.io.File
import java.lang.Thread
import scala.annotation.internal.sharable
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.runner.ObjectRunner
import dotty.tools.dotc.config.Properties.envOrNone
import dotty.tools.io.Jar
import dotty.tools.runner.ScalaClassLoader
import java.nio.file.Paths
import dotty.tools.dotc.config.CommandLineParser
import dotty.tools.scripting.StringDriver

enum ExecuteMode:
  case Guess
  case Script
  case Repl
  case Run
  case PossibleRun
  case Expression

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
  targetExpression: String = "",
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

  def withExpression(scalaSource: String): Settings =
    this.copy(targetExpression = scalaSource)

  def withSave: Settings =
    this.copy(save = true)

  def noSave: Settings =
    this.copy(save = false)

  def withModeShouldBePossibleRun: Settings =
    this.copy(modeShouldBePossibleRun = true)

  def withModeShouldBeRun: Settings =
    this.copy(modeShouldBeRun = true)

  def withCompiler: Settings =
    this.copy(compiler = true)
}

object MainGenericRunner {

  val classpathSeparator = File.pathSeparator

  def processClasspath(cp: String, tail: List[String]): (List[String], List[String]) =
    val cpEntries = cp.split(classpathSeparator).toList
    val singleEntryClasspath: Boolean = cpEntries.take(2).size == 1
    val globdir: String = if singleEntryClasspath then cp.replaceAll("[\\\\/][^\\\\/]*$", "") else "" // slash/backslash agnostic
    def validGlobbedJar(s: String): Boolean = s.startsWith(globdir) && ((s.toLowerCase.endsWith(".jar") || s.toLowerCase.endsWith(".zip")))
    if singleEntryClasspath && validGlobbedJar(cpEntries.head) then
      // reassemble globbed wildcard classpath
      // globdir is wildcard directory for globbed jar files, reconstruct the intended classpath
      val cpJars = tail.takeWhile( f => validGlobbedJar(f) )
      val remainingArgs = tail.drop(cpJars.size)
      (remainingArgs, cpEntries ++ cpJars)
    else
      (tail, cpEntries)

  @sharable val javaOption = raw"""-J(.*)""".r
  @sharable val scalaOption = raw"""@.*""".r
  @sharable val colorOption = raw"""-color:.*""".r
  @tailrec
  def processArgs(args: List[String], settings: Settings): Settings = args match
    case Nil =>
      settings
    case "-run" :: fqName :: tail =>
      processArgs(tail, settings.withExecuteMode(ExecuteMode.Run).withTargetToRun(fqName))
    case ("-cp" | "-classpath" | "--class-path") :: cp :: tail =>
      val (tailargs, newEntries) = processClasspath(cp, tail)
      processArgs(tailargs, settings.copy(classPath = settings.classPath ++ newEntries.filter(_.nonEmpty)))
    case ("-version" | "--version") :: _ =>
      settings.copy(
        executeMode = ExecuteMode.Repl,
        residualArgs = List("-version")
      )
    case ("-v" | "-verbose" | "--verbose") :: tail =>
      processArgs(
        tail,
        settings.copy(
          verbose = true,
          residualArgs = settings.residualArgs :+ "-verbose"
        )
      )
    case "-save" :: tail =>
      processArgs(tail, settings.withSave)
    case "-nosave" :: tail =>
      processArgs(tail, settings.noSave)
    case "-with-compiler" :: tail =>
      processArgs(tail, settings.withCompiler)
    case (o @ javaOption(striped)) :: tail =>
      processArgs(tail, settings.withJavaArgs(striped).withScalaArgs(o))
    case (o @ scalaOption(_*)) :: tail =>
      val remainingArgs = (CommandLineParser.expandArg(o) ++ tail).toList
      processArgs(remainingArgs, settings)
    case (o @ colorOption(_*)) :: tail =>
      processArgs(tail, settings.withScalaArgs(o))
    case "-e" :: expression :: tail =>
      val mainSource = s"@main def main(args: String *): Unit =\n  ${expression}"
      settings
        .withExecuteMode(ExecuteMode.Expression)
        .withExpression(mainSource)
        .withScriptArgs(tail*)
        .noSave // -save not useful here
    case arg :: tail =>
      val line = Try(Source.fromFile(arg).getLines.toList).toOption.flatMap(_.headOption)
      lazy val hasScalaHashbang = { val s = line.getOrElse("") ; s.startsWith("#!") && s.contains("scala") }
      if arg.endsWith(".scala") || arg.endsWith(".sc") || hasScalaHashbang then
        settings
          .withExecuteMode(ExecuteMode.Script)
          .withTargetScript(arg)
          .withScriptArgs(tail*)
      else
        val newSettings = if arg.startsWith("-") then settings else settings.withPossibleEntryPaths(arg).withModeShouldBePossibleRun
        processArgs(tail, newSettings.withResidualArgs(arg))
  end processArgs

  def process(args: Array[String]): Boolean =
    val scalaOpts = envOrNone("SCALA_OPTS").toArray.flatMap(_.split(" ")).filter(_.nonEmpty)
    val allArgs = scalaOpts ++ args
    val settings = processArgs(allArgs.toList, Settings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)

    def removeCompiler(cp: Array[String]) =
      if (!settings.compiler) then // Let's remove compiler from the classpath
        val compilerLibs = Seq("scala3-compiler", "scala3-interfaces", "tasty-core", "scala-asm", "scala3-staging", "scala3-tasty-inspector")
        cp.filterNot(c => compilerLibs.exists(c.contains))
      else
        cp

    def run(settings: Settings): Option[Throwable] = settings.executeMode match
      case ExecuteMode.Repl =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)
        None

      case ExecuteMode.PossibleRun =>
        val newClasspath = (settings.classPath :+ ".").flatMap(_.split(classpathSeparator).filter(_.nonEmpty)).map(File(_).toURI.toURL)
        import dotty.tools.runner.RichClassLoader._
        val newClassLoader = ScalaClassLoader.fromURLsParallelCapable(newClasspath)
        val targetToRun = settings.possibleEntryPaths.to(LazyList).find { entryPath =>
          newClassLoader.tryToLoadClass(entryPath).orElse {
            Option.when(Jar.isJarOrZip(dotty.tools.io.Path(entryPath)))(Jar(entryPath).mainClass).flatten
          }.isDefined
        }
        val newSettings = targetToRun match
          case Some(fqName) =>
            settings.withTargetToRun(fqName).copy(residualArgs = settings.residualArgs.filterNot(fqName.==)).withExecuteMode(ExecuteMode.Run)
          case None =>
            settings.withExecuteMode(ExecuteMode.Repl)
        run(newSettings)

      case ExecuteMode.Run =>
        val scalaClasspath = ClasspathFromClassloader(Thread.currentThread().getContextClassLoader).split(classpathSeparator)
        val newClasspath = (settings.classPath.flatMap(_.split(classpathSeparator).filter(_.nonEmpty)) ++ removeCompiler(scalaClasspath) :+ ".").map(File(_).toURI.toURL)
        ObjectRunner.runAndCatch(newClasspath, settings.targetToRun, settings.residualArgs).flatMap {
          case ex: ClassNotFoundException if ex.getMessage == settings.targetToRun =>
            val file = settings.targetToRun
            Jar(file).mainClass match
              case Some(mc) =>
                ObjectRunner.runAndCatch(newClasspath :+ File(file).toURI.toURL, mc, settings.residualArgs)
              case None =>
                Some(IllegalArgumentException(s"No main class defined in manifest in jar: $file"))
          case ex => Some(ex)
        }

      case ExecuteMode.Script =>
        val targetScript = Paths.get(settings.targetScript).toFile
        val targetJar = settings.targetScript.replaceAll("[.][^\\/]*$", "")+".jar"
        val precompiledJar = File(targetJar)
        val mainClass = if !precompiledJar.isFile then "" else Jar(targetJar).mainClass.getOrElse("")
        val jarIsValid = mainClass.nonEmpty && precompiledJar.lastModified >= targetScript.lastModified && settings.save
        if jarIsValid then
          // precompiledJar exists, is newer than targetScript, and manifest defines a mainClass
          sys.props("script.path") = targetScript.toPath.toAbsolutePath.normalize.toString
          val scalaClasspath = ClasspathFromClassloader(Thread.currentThread().getContextClassLoader).split(classpathSeparator)
          val newClasspath = (settings.classPath.flatMap(_.split(classpathSeparator).filter(_.nonEmpty)) ++ removeCompiler(scalaClasspath) :+ ".").map(File(_).toURI.toURL)
          if mainClass.nonEmpty then
            ObjectRunner.runAndCatch(newClasspath :+ File(targetJar).toURI.toURL, mainClass, settings.scriptArgs)
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
          None

      case ExecuteMode.Expression =>
        val cp = settings.classPath match {
          case Nil => ""
          case list => list.mkString(classpathSeparator)
        }
        val cpArgs = if cp.isEmpty then Nil else List("-classpath", cp)
        val properArgs = cpArgs ++ settings.residualArgs ++ settings.scalaArgs
        val driver = StringDriver(properArgs.toArray, settings.targetExpression)
        driver.compileAndRun(settings.classPath)

      case ExecuteMode.Guess =>
        if settings.modeShouldBePossibleRun then
          run(settings.withExecuteMode(ExecuteMode.PossibleRun))
        else if settings.modeShouldBeRun then
          run(settings.withExecuteMode(ExecuteMode.Run))
        else
          run(settings.withExecuteMode(ExecuteMode.Repl))

    run(settings) match
      case e @ Some(ex) => errorFn("", e)
      case _            => true

  def errorFn(str: String, e: Option[Throwable] = None): Boolean =
    if (str.nonEmpty) Console.err.println(str)
    e.foreach(_.printStackTrace())
    false

  def main(args: Array[String]): Unit =
    if (!process(args)) System.exit(1)

}
