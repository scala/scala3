package dotty.tools.scripting

import java.io.File

/** Main entry point to the Scripting execution engine */
object Main:
  /** All arguments before -script <target_script> are compiler arguments.
      All arguments afterwards are script arguments.*/
  def distinguishArgs(args: Array[String]): (Array[String], File, Array[String]) =
    val (compilerArgs, rest) = args.splitAt(args.indexOf("-script"))
    val file = File(rest(1))
    val scriptArgs = rest.drop(2)
    (compilerArgs, file, scriptArgs)
  end distinguishArgs

  def main(args: Array[String]): Unit =
    val (compilerArgs, scriptFile, scriptArgs) = distinguishArgs(args)
    try ScriptingDriver(compilerArgs, scriptFile, scriptArgs).compileAndRun()
    catch
      case ScriptingException(msg) =>
        println(s"Error: $msg")
        sys.exit(1)
