package dotty.tools.scripting

import java.io.File
import java.nio.file.Path

/** Main entry point to the Scripting execution engine */
object Main:
  /** All arguments before -script <target_script> are compiler arguments.
      All arguments afterwards are script arguments.*/
  def distinguishArgs(args: Array[String]): (Array[String], File, Array[String]) =
    args.foreach { printf("arg[%s]\n",_) }
    val (compilerArgs, rest) = args.splitAt(args.indexOf("-script"))
    if( rest.isEmpty ){
      sys.error(s"missing: -script <scriptName>")
    }
    val file = File(rest.take(1).mkString)
    val scriptArgs = rest.drop(2)
    (compilerArgs, file, scriptArgs)
  end distinguishArgs

  def main(args: Array[String]): Unit =
    val (compilerArgs, scriptFile, scriptArgs) = distinguishArgs(args)
    try ScriptingDriver(compilerArgs, scriptFile, scriptArgs).compileAndRun{ (tmpDir:Path,classpath:String) =>
      printf("%s\n",tmpDir.toString)
      printf("%s\n",classpath)
    }
    catch
      case ScriptingException(msg) =>
        println(s"Error: $msg")
        sys.exit(1)
