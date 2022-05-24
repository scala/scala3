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

enum CompileMode:
  case Guess
  case Compile
  case Decompile
  case PrintTasty
  case Script
  case Repl
  case Run

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

  val classpathSeparator = File.pathSeparator

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
    case "-repl" :: tail =>
      process(tail, settings.withCompileMode(CompileMode.Repl))
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
    case "-run" :: tail =>
      process(tail, settings.withCompileMode(CompileMode.Run))
    case "-colors" :: tail =>
      process(tail, settings.withColors)
    case "-no-colors" :: tail =>
      process(tail, settings.withNoColors)
    case "-with-compiler" :: tail =>
      process(tail, settings.withCompiler)
    case ("-cp" | "-classpath" | "--class-path") :: cp :: tail =>
      val (tailargs, newEntries) = MainGenericRunner.processClasspath(cp, tail)
      process(tailargs, settings.copy(classPath = settings.classPath ++ newEntries.filter(_.nonEmpty)))
    case "-Oshort" :: tail =>
      // Nothing is to be done here. Request that the user adds the relevant flags manually.
      // i.e this has no effect when MainGenericRunner is invoked programatically.
      val addTC="-XX:+TieredCompilation"
      val tStopAtLvl="-XX:TieredStopAtLevel=1"
      println(s"ignoring deprecated -Oshort flag, please add `-J$addTC` and `-J$tStopAtLvl` flags manually")
      process(tail, settings)
    case javaOption(stripped) :: tail =>
      process(tail, settings.withJavaArgs(stripped))
    case javaPropOption(opt, value) :: tail =>
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
      case CompileMode.Repl | CompileMode.Run =>
        addJavaProps()
        val properArgs = reconstructedArgs()
        repl.Main.main(properArgs.toArray)
      case CompileMode.Guess =>
        run(settings.withCompileMode(CompileMode.Compile))
    end run

    run(settings)
  end main
}
