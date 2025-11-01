package dotty.tools

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

enum CompileMode:
  case Guess
  case Compile
  case Decompile
  case PrintTasty
  case Script

case class CompileSettings(
  verbose: Boolean = false,
  classPath: List[String] = List.empty,
  compileMode: CompileMode = CompileMode.Guess,
  exitCode: Int = 0,
  javaArgs: List[String] = List.empty,
  javaProps: List[(String, String)] = List.empty,
  scalaArgs: List[String] = List.empty,
  residualArgs: List[String] = List.empty,
  scriptArgs: List[String] = List.empty,
  targetScript: String = "",
  compiler: Boolean = false,
  quiet: Boolean = false,
  colors: Boolean = false,
) {
  def withCompileMode(em: CompileMode): CompileSettings = this.compileMode match
    case CompileMode.Guess =>
      this.copy(compileMode = em)
    case _ =>
      println(s"compile_mode==[$compileMode], attempted overwrite by [$em]")
      this.copy(exitCode = 1)
  end withCompileMode

  def withScalaArgs(args: String*): CompileSettings =
    this.copy(scalaArgs = scalaArgs.appendedAll(args.toList.filter(_.nonEmpty)))

  def withJavaArgs(args: String*): CompileSettings =
    this.copy(javaArgs = javaArgs.appendedAll(args.toList.filter(_.nonEmpty)))

  def withJavaProps(args: (String, String)*): CompileSettings =
    this.copy(javaProps = javaProps.appendedAll(args.toList))

  def withResidualArgs(args: String*): CompileSettings =
    this.copy(residualArgs = residualArgs.appendedAll(args.toList.filter(_.nonEmpty)))

  def withScriptArgs(args: String*): CompileSettings =
    this.copy(scriptArgs = scriptArgs.appendedAll(args.toList.filter(_.nonEmpty)))

  def withTargetScript(file: String): CompileSettings =
    Try(Source.fromFile(file)).toOption match
      case Some(_) => this.copy(targetScript = file)
      case None      =>
        println(s"not found $file")
        this.copy(exitCode = 2)
  end withTargetScript

  def withCompiler: CompileSettings =
    this.copy(compiler = true)

  def withQuiet: CompileSettings =
    this.copy(quiet = true)

  def withColors: CompileSettings =
    this.copy(colors = true)

  def withNoColors: CompileSettings =
    this.copy(colors = false)
}

object MainGenericCompiler {

  val classpathSeparator: String = File.pathSeparator

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
  @sharable val javaPropOption = raw"""-D(.+?)=(.?)""".r
  @tailrec
  def process(args: List[String], settings: CompileSettings): CompileSettings = args match
    case Nil =>
      settings
    case "--" :: tail =>
      process(Nil, settings.withResidualArgs(tail.toList*))
    case ("-v" | "-verbose" | "--verbose") :: tail =>
      process(tail, settings.withScalaArgs("-verbose"))
    case ("-q" | "-quiet") :: tail =>
      process(tail, settings.withQuiet)
    case "-script" :: targetScript :: tail =>
      process(Nil, settings
        .withCompileMode(CompileMode.Script)
        .withJavaProps("script.path" -> targetScript)
        .withTargetScript(targetScript)
        .withScriptArgs(tail*))
    case "-compile" :: tail =>
      process(tail, settings.withCompileMode(CompileMode.Compile))
    case "-decompile" :: tail =>
      process(tail, settings.withCompileMode(CompileMode.Decompile))
    case "-print-tasty" :: tail =>
      process(tail, settings.withCompileMode(CompileMode.PrintTasty))
    case "-colors" :: tail =>
      process(tail, settings.withColors)
    case "-no-colors" :: tail =>
      process(tail, settings.withNoColors)
    case "-with-compiler" :: tail =>
      process(tail, settings.withCompiler)
    case ("-cp" | "-classpath" | "--class-path") :: cp :: tail =>
      val (tailargs, newEntries) = processClasspath(cp, tail)
      process(tailargs, settings.copy(classPath = settings.classPath ++ newEntries.filter(_.nonEmpty)))
    case "-Oshort" :: tail =>
      // Nothing is to be done here. Request that the user adds the relevant flags manually.
      val addTC="-XX:+TieredCompilation"
      val tStopAtLvl="-XX:TieredStopAtLevel=1"
      println(s"ignoring deprecated -Oshort flag, please add `-J$addTC` and `-J$tStopAtLvl` flags manually")
      process(tail, settings)
    case javaOption(stripped: String) :: tail =>
      process(tail, settings.withJavaArgs(stripped))
    case javaPropOption(opt: String, value: String) :: tail =>
      process(tail, settings.withJavaProps(opt -> value))
    case arg :: tail =>
      process(tail, settings.withResidualArgs(arg))
  end process

  def main(args: Array[String]): Unit =
    val settings = process(args.toList, CompileSettings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)

    def classpathSetting =
      if settings.classPath.isEmpty then List()
      else List("-classpath", settings.classPath.mkString(classpathSeparator))

    def reconstructedArgs() =
      classpathSetting ++ settings.scalaArgs ++ settings.residualArgs

    def addJavaProps(): Unit =
      settings.javaProps.foreach { (k, v) => sys.props(k) = v }

    def run(settings: CompileSettings): Unit = settings.compileMode match
      case CompileMode.Compile =>
        addJavaProps()
        val properArgs = reconstructedArgs()
        dotty.tools.dotc.Main.main(properArgs.toArray)
      case CompileMode.Decompile =>
        addJavaProps()
        val properArgs = reconstructedArgs()
        dotty.tools.dotc.decompiler.Main.main(properArgs.toArray)
      case CompileMode.PrintTasty =>
        addJavaProps()
        val properArgs = reconstructedArgs()
        dotty.tools.dotc.core.tasty.TastyPrinter.main(properArgs.toArray)
      case CompileMode.Script => // Naive copy from scalac bash script
        addJavaProps()
        val properArgs =
          reconstructedArgs()
          ++ List("-script", settings.targetScript)
          ++ settings.scriptArgs
        scripting.Main.main(properArgs.toArray)
      case CompileMode.Guess =>
        run(settings.withCompileMode(CompileMode.Compile))
    end run

    run(settings)
  end main
}
