package dotty.tools

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex
import java.net.URLClassLoader

enum ExecuteMode:
  case Guess
  case Script
  case Repl
  case Run

case class Settings(
  verbose: Boolean = false,
  classPath: List[String] = List.empty,
  executeMode: ExecuteMode = ExecuteMode.Guess,
  exitCode: Int = 0,
  residualArgs: List[String] = List.empty,
  scriptArgs: List[String] = List.empty,
  targetScript: String = "",
) {
  def withExecuteMode(em: ExecuteMode): Settings = this.executeMode match
    case ExecuteMode.Guess =>
      this.copy(executeMode = em)
    case _ =>
      println(s"execute_mode==[$executeMode], attempted overwrite by [$em]")
      this.copy(exitCode = 1)
  end withExecuteMode

  def withResidualArgs(args: String*): Settings =
    this.copy(residualArgs = residualArgs.appendedAll(args.toList))

  def withScriptArgs(args: String*): Settings =
    this.copy(scriptArgs = scriptArgs.appendedAll(args.toList))

  def withTargetScript(file: String): Settings =
    Try(Source.fromFile(file)).toOption match
      case Some(_) => this.copy(targetScript = file)
      case None      =>
        println(s"not found $file")
        this.copy(exitCode = 2)
  end withTargetScript
}

object MainGenericRunner {

  val shebangscala: Regex = """#!.*scala""".r

  @tailrec
  def process(args: List[String], settings: Settings): Settings = args match
    case Nil =>
      settings
    case "-repl" :: tail =>
      process(tail, settings.withExecuteMode(ExecuteMode.Repl))
    case "-run" :: tail =>
      process(tail, settings.withExecuteMode(ExecuteMode.Run))
    case ("-cp" | "-classpath" | "--classpath") :: cp :: tail =>
      process(tail, settings.copy(classPath = settings.classPath.appended(cp)))
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
    case arg :: tail =>
      val line = Try(Source.fromFile(arg).getLines.toList).toOption.flatMap(_.headOption)
      val newSettings = if arg.endsWith(".scala") || arg.endsWith(".sc") || (line.nonEmpty && shebangscala.matches(line.get))
        then {
          settings
            .withExecuteMode(ExecuteMode.Script)
            .withTargetScript(arg)
            .withScriptArgs(tail*)
        } else
          settings.withResidualArgs(arg)
      process(tail, newSettings.withResidualArgs(arg))

  def main(args: Array[String]): Unit =
    val settings = process(args.toList, Settings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)
    settings.executeMode match
      case ExecuteMode.Repl =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(";")).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)
      case ExecuteMode.Run =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(";")).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        //TODO this is just a java proxy?
      case ExecuteMode.Script =>
        val properArgs =
          List("classpath", settings.classPath.mkString(";")).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
            ++ List("-script", settings.targetScript)
            ++ settings.scriptArgs
        scripting.Main.main(properArgs.toArray)
      case ExecuteMode.Guess =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(";")).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)
}
