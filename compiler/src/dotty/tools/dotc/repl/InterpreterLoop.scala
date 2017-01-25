package dotty.tools
package dotc
package repl

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.io.IOException
import java.lang.{ClassLoader, System, Thread}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.Duration
import reporting.Reporter
import core._
import Contexts._
import annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

/** The interactive shell.  It provides a read-eval-print loop around
 *  the Interpreter class.
 *  After instantiation, clients should call the `run` method.
 *
 *  @author Moez A. Abdel-Gawad
 *  @author Lex Spoon
 *  @author Martin Odersky
 */
class InterpreterLoop(compiler: Compiler, config: REPL.Config)(implicit ctx: Context) {
  import config._

  val interpreter = compiler.asInstanceOf[Interpreter]

  private var in = input(interpreter)

  /** The context class loader at the time this object was created */
  protected val originalClassLoader =
    Thread.currentThread.getContextClassLoader

  /** A reverse list of commands to replay if the user
    * requests a :replay */
  var replayCommandsRev: List[String] = Nil

  /** A list of commands to replay if the user requests a :replay */
  def replayCommands = replayCommandsRev.reverse

  /** Record a command for replay should the user request a :replay */
  def addReplay(cmd: String) =
    replayCommandsRev = cmd :: replayCommandsRev

  /** Close the interpreter */
  def closeInterpreter()(implicit ctx: Context): Unit = {
    ctx.reporter.flush()
    Thread.currentThread.setContextClassLoader(originalClassLoader)
  }

  /** print a friendly help message */
  def printHelp(): Unit = {
    printWelcome()
    output.println("Type :load followed by a filename to load a Scala file.")
    output.println("Type :replay to reset execution and replay all previous commands.")
    output.println("Type :quit to exit the interpreter.")
  }

  /** Print a welcome message */
  def printWelcome(): Unit = {
    output.println(s"Welcome to Scala$version " + " (" +
                System.getProperty("java.vm.name") + ", Java " + System.getProperty("java.version") + ")." )
    output.println("Type in expressions to have them evaluated.")
    output.println("Type :help for more information.")
    output.flush()
  }

  val gitHash = ManifestInfo.attributes.getOrElse("Git-Hash", "unknown")
  val version = s".next (pre-alpha, git-hash: $gitHash)"

  /** The main read-eval-print loop for the interpreter.  It calls
   *  `command()` for each line of input.
   */
  @tailrec final def repl(line: String = in.readLine(prompt)): Unit =
    if (line != null) {
      val (keepGoing, finalLineOpt) = command(line)
      if (keepGoing) {
        finalLineOpt.foreach(addReplay)
        output.flush()
        repl()
      }
    }

  /** interpret all lines from a specified file */
  def interpretAllFrom(filename: String): Unit = {
    import java.nio.file.{Files, Paths}
    import scala.collection.JavaConversions._
    try {
      val lines = Files.readAllLines(Paths.get(filename)).mkString("\n")
      output.println("Loading " + filename + "...")
      output.flush
      interpreter.interpret(lines)
    } catch {
      case _: IOException =>
        output.println("Error opening file: " + filename)
    }
  }

  /** create a new interpreter and replay all commands so far */
  def replay(): Unit = {
    for (cmd <- replayCommands) {
      output.println("Replaying: " + cmd)
      output.flush()  // because maybe cmd will have its own output
      command(cmd)
      output.println
    }
  }

  /** Run one command submitted by the user.  Three values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): (Boolean, Option[String]) = {
    def withFile(command: String)(action: String => Unit): Unit = {
      val spaceIdx = command.indexOf(' ')
      if (spaceIdx <= 0) {
        output.println("That command requires a filename to be specified.")
        return
      }
      val filename = command.substring(spaceIdx).trim
      if (!new File(filename).exists) {
        output.println("That file does not exist")
        return
      }
      action(filename)
    }

    val helpRegexp    = ":h(e(l(p)?)?)?"
    val quitRegexp    = ":q(u(i(t)?)?)?"
    val loadRegexp    = ":l(o(a(d)?)?)?.*"
    val replayRegexp  = ":r(e(p(l(a(y)?)?)?)?)?.*"
    val lastOutput    = interpreter.lastOutput()

    var shouldReplay: Option[String] = None

    if (line.matches(helpRegexp))
      printHelp()
    else if (line.matches(quitRegexp))
      return (false, None)
    else if (line.matches(loadRegexp)) {
      withFile(line)(f => {
        interpretAllFrom(f)
        shouldReplay = Some(line)
      })
    }
    else if (line matches replayRegexp)
      replay()
    else if (line startsWith ":")
      output.println("Unknown command.  Type :help for help.")
    else
      shouldReplay = lastOutput match { // don't interpret twice
        case Nil => interpretStartingWith(line)
        case oldRes =>
          oldRes foreach output.print
          Some(line)
      }

    (true, shouldReplay)
  }

  def silentlyRun(cmds: List[String]): Unit = cmds.foreach { cmd =>
    interpreter.beQuietDuring(interpreter.interpret(cmd))
  }

  def silentlyBind(values: Array[(String, Any)]): Unit = values.foreach { case (id, value) =>
    interpreter.beQuietDuring(
      interpreter.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value.asInstanceOf[AnyRef]))
  }

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the full string
    * to be recorded for replay, if any.
    */
  def interpretStartingWith(code: String): Option[String] =
    interpreter.interpret(code) match {
      case Interpreter.Success => Some(code)
      case _ => None
    }
/*
  def loadFiles(settings: Settings) {
    settings match {
      case settings: GenericRunnerSettings =>
        for (filename <- settings.loadfiles.value) {
          val cmd = ":load " + filename
          command(cmd)
          replayCommandsRev = cmd :: replayCommandsRev
          output.println()
        }
      case _ =>
    }
  }
*/
  def run(): Reporter = {
    // loadFiles(settings)
    try {
      if (!ctx.reporter.hasErrors) { // if there are already errors, no sense to continue
        printWelcome()
        silentlyRun(config.initialCommands)
        silentlyBind(config.boundValues)
        repl(in.readLine(prompt))
        silentlyRun(config.cleanupCommands)
      }
    } finally {
      closeInterpreter()
    }
    ctx.reporter
  }
}
