package dotty.tools

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Try, Success, Failure}
import java.io.File

enum CompileMode:
  case Guess
  case Compile
  case Decompile
  case PrintTasty
  case Script

case class CompileSettings(
  classPath: Vector[String] = Vector.empty,
  compileMode: CompileMode = CompileMode.Guess,
  exitCode: Int = 0,
  javaProps: Vector[(String, String)] = Vector.empty,
  scalaArgs: Vector[String] = Vector.empty,
  residualArgs: Vector[String] = Vector.empty,
  scriptArgs: Vector[String] = Vector.empty,
  targetScript: String = "",
) {
  def withCompileMode(em: CompileMode): CompileSettings = this.compileMode match
    case CompileMode.Guess =>
      this.copy(compileMode = em)
    case _ =>
      println(s"compile_mode==[$compileMode], attempted overwrite by [$em]")
      this.copy(exitCode = 1)
  end withCompileMode

  def withScalaArgs(args: String*): CompileSettings =
    this.copy(scalaArgs = scalaArgs.appendedAll(args.toVector.filter(_.nonEmpty)))

  def withJavaProps(args: (String, String)*): CompileSettings =
    this.copy(javaProps = javaProps.appendedAll(args.toVector))

  def withResidualArgs(args: String*): CompileSettings =
    this.copy(residualArgs = residualArgs.appendedAll(args.toVector.filter(_.nonEmpty)))

  def withScriptArgs(args: String*): CompileSettings =
    this.copy(scriptArgs = scriptArgs.appendedAll(args.toVector.filter(_.nonEmpty)))

  def withTargetScript(file: String): CompileSettings =
    Try(Source.fromFile(file)) match
      case Success(_) => this.copy(targetScript = file)
      case Failure(_) =>
        println(s"not found $file")
        this.copy(exitCode = 2)
  end withTargetScript
}

object MainGenericCompiler {

  private val classpathSeparator: String = File.pathSeparator
  private val javaPropOption = raw"""-D(.+?)=(.?)""".r

  private def processClasspath(cp: String, tail: Vector[String]): (Vector[String], Vector[String]) =
    val cpEntries = cp.split(classpathSeparator).toVector
    val singleEntryClasspath: Boolean = cpEntries.take(2).size == 1
    val globDir: String = if singleEntryClasspath then cp.replaceAll("[\\\\/][^\\\\/]*$", "") else "" // slash/backslash agnostic
    def validGlobbedJar(s: String): Boolean = s.startsWith(globDir) && (s.toLowerCase.endsWith(".jar") || s.toLowerCase.endsWith(".zip"))
    if singleEntryClasspath && validGlobbedJar(cpEntries.head) then
      // reassemble globbed wildcard classpath
      // globDir is wildcard directory for globbed jar files, reconstruct the intended classpath
      val cpJars = tail.takeWhile(validGlobbedJar)
      val remainingArgs = tail.drop(cpJars.size)
      (remainingArgs, cpEntries ++ cpJars)
    else
      (tail, cpEntries)

  @tailrec
  def process(args: Vector[String], settings: CompileSettings): CompileSettings = (args: @unchecked) match
    case Vector() =>
      settings
    case "--" +: tail =>
      settings.withResidualArgs(tail*)
    case ("-v" | "-verbose" | "--verbose") +: tail =>
      process(tail, settings.withScalaArgs("-verbose"))
    case "-script" +: targetScript +: tail =>
      process(Vector(), settings
        .withCompileMode(CompileMode.Script)
        .withJavaProps("script.path" -> targetScript)
        .withTargetScript(targetScript)
        .withScriptArgs(tail*))
    case "-compile" +: tail =>
      process(tail, settings.withCompileMode(CompileMode.Compile))
    case "-decompile" +: tail =>
      process(tail, settings.withCompileMode(CompileMode.Decompile))
    case "-print-tasty" +: tail =>
      process(tail, settings.withCompileMode(CompileMode.PrintTasty))
    case ("-cp" | "-classpath" | "--class-path") +: cp +: tail =>
      val (tailArgs, newEntries) = processClasspath(cp, tail)
      process(tailArgs, settings.copy(classPath = settings.classPath ++ newEntries.filter(_.nonEmpty)))
    case "-Oshort" +: tail =>
      // Nothing is to be done here. Request that the user adds the relevant flags manually.
      val addTC="-XX:+TieredCompilation"
      val tStopAtLvl="-XX:TieredStopAtLevel=1"
      println(s"ignoring deprecated -Oshort flag, please add `-J$addTC` and `-J$tStopAtLvl` flags manually")
      process(tail, settings)
    case javaPropOption(opt: String, value: String) +: tail =>
      process(tail, settings.withJavaProps(opt -> value))
    case arg +: tail =>
      process(tail, settings.withResidualArgs(arg))
  end process

  def main(args: Array[String]): Unit =
    val settings = process(args.toVector, CompileSettings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)

    val classpathSetting =
      if settings.classPath.isEmpty then Vector()
      else Vector("-classpath", settings.classPath.mkString(classpathSeparator))

    val properArgs = classpathSetting ++ settings.scalaArgs ++ settings.residualArgs

    settings.javaProps.foreach { (k, v) => sys.props(k) = v }

    settings.compileMode match
      case CompileMode.Guess | CompileMode.Compile =>
        dotty.tools.dotc.Main.main(properArgs.toArray)
      case CompileMode.Decompile =>
        dotty.tools.dotc.decompiler.Main.main(properArgs.toArray)
      case CompileMode.PrintTasty =>
        dotty.tools.dotc.core.tasty.TastyPrinter.main(properArgs.toArray)
      case CompileMode.Script => // Naive copy from scalac bash script
        val fullArgs =
          properArgs
          ++ Vector("-script", settings.targetScript)
          ++ settings.scriptArgs
        scripting.Main.main(fullArgs.toArray)
  end main
}
