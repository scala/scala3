package dotty.tools


import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import java.net.URLClassLoader
import sys.process._
import java.io.File
import java.lang.Thread

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
  areWithCompiler: Boolean = false,
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

  def withCompiler: Settings =
    this.copy(areWithCompiler = true)
}

object MainGenericRunner {

  final val classpathSeparator = ":"

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
    case "-with-compiler" :: tail =>
      process(tail, settings.withCompiler)
    case arg :: tail =>
      val line = Try(Source.fromFile(arg).getLines.toList).toOption.flatMap(_.headOption)
      if arg.endsWith(".scala") || arg.endsWith(".sc") || (line.nonEmpty && raw"#!.*scala".r.matches(line.get)) then
        settings
          .withExecuteMode(ExecuteMode.Script)
          .withTargetScript(arg)
          .withScriptArgs(tail*)
      else
        process(tail, settings.withResidualArgs(arg))

  def main(args: Array[String]): Unit =
    val settings = process(args.toList, Settings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)
    settings.executeMode match
      case ExecuteMode.Repl =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)
      case ExecuteMode.Run =>
        val properArgs =
          val newClasspath = settings.classPath ++ getClasspath :+ "."
          List("-classpath", newClasspath.mkString(classpathSeparator)).filter(Function.const(newClasspath.nonEmpty))
            ++ settings.residualArgs
        s"java ${properArgs.mkString(" ")}".! // For now we collect classpath that coursier provides for convenience
      case ExecuteMode.Script =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
            ++ List("-script", settings.targetScript)
            ++ settings.scriptArgs
        scripting.Main.main(properArgs.toArray)
      case ExecuteMode.Guess =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)


  private def getClasspath(cl: ClassLoader): Array[String] = cl match
    case null => Array()
    case u: URLClassLoader => u.getURLs.map(_.toURI.toString) ++ getClasspath(cl.getParent)
    case cl if cl.getClass.getName == "jdk.internal.loader.ClassLoaders$AppClassLoader" =>
      // Required with JDK >= 9
      sys.props.getOrElse("java.class.path", "")
        .split(File.pathSeparator)
        .filter(_.nonEmpty)
    case _ => getClasspath(cl.getParent)

  private def getClasspath: List[String] =
    getClasspath(Thread.currentThread().getContextClassLoader).toList
}
