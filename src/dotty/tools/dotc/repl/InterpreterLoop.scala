package dotty.tools
package dotc
package repl

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.io.IOException
import java.lang.{ClassLoader, System}
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
 *  @author  Lex Spoon
 *  @author Martin Odersky
 */
class InterpreterLoop(
    compiler: Compiler,
    private var in: InteractiveReader,
    out: PrintWriter)(implicit ctx: Context) {

  val interpreter = compiler.asInstanceOf[Interpreter]

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
    out.println("Type :load followed by a filename to load a Scala file.")
    out.println("Type :replay to reset execution and replay all previous commands.")
    out.println("Type :quit to exit the interpreter.")
  }

  /** Print a welcome message */
  def printWelcome(): Unit = {
    out.println(s"Welcome to Scala$version " + " (" +
                System.getProperty("java.vm.name") + ", Java " + System.getProperty("java.version") + ")." )
    out.println("Type in expressions to have them evaluated.")
    out.println("Type :help for more information.")
    out.flush()
  }

  /** Prompt to print when awaiting input */
  val prompt             = "scala> "
  val continuationPrompt = "     | "

  val version = ".next (pre-alpha)"

  /** The first interpreted command always takes a couple of seconds
   *  due to classloading. To bridge the gap, we warm up the interpreter
   *  by letting it interpret a dummy line while waiting for the first
   *  line of input to be entered.
   */
  def firstLine(): String = {
    val futLine = Future(in.readLine(prompt))
    interpreter.beQuietDuring(
      interpreter.interpret("val theAnswerToLifeInTheUniverseAndEverything = 21 * 2"))
    Await.result(futLine, Duration.Inf)
  }

  /** The main read-eval-print loop for the interpreter.  It calls
   *  `command()` for each line of input.
   */
  @tailrec final def repl(line: String = in.readLine(prompt)): Unit =
    if (line != null) {
      val (keepGoing, finalLineOpt) = command(line)
      if (keepGoing) {
        finalLineOpt.foreach(addReplay)
        out.flush()
        repl()
      }
    }

  /** interpret all lines from a specified file */
  def interpretAllFrom(filename: String): Unit = {
    val fileIn = try {
      new FileReader(filename)
    } catch {
      case _: IOException =>
        out.println("Error opening file: " + filename)
        return
    }
    val oldIn = in
    val oldReplay = replayCommandsRev
    try {
      val inFile = new BufferedReader(fileIn)
      in = new SimpleReader(inFile, out, false)
      out.println("Loading " + filename + "...")
      out.flush
      repl()
    } finally {
      in = oldIn
      replayCommandsRev = oldReplay
      fileIn.close
    }
  }

  /** create a new interpreter and replay all commands so far */
  def replay(): Unit = {
    for (cmd <- replayCommands) {
      out.println("Replaying: " + cmd)
      out.flush()  // because maybe cmd will have its own output
      command(cmd)
      out.println
    }
  }

  /** Run one command submitted by the user.  Three values are returned:
    * (1) whether to keep running, (2) the line to record for replay,
    * if any. */
  def command(line: String): (Boolean, Option[String]) = {
    def withFile(command: String)(action: String => Unit): Unit = {
      val spaceIdx = command.indexOf(' ')
      if (spaceIdx <= 0) {
        out.println("That command requires a filename to be specified.")
        return
      }
      val filename = command.substring(spaceIdx).trim
      if (!new File(filename).exists) {
        out.println("That file does not exist")
        return
      }
      action(filename)
    }

    val helpRegexp    = ":h(e(l(p)?)?)?"
    val quitRegexp    = ":q(u(i(t)?)?)?"
    val loadRegexp    = ":l(o(a(d)?)?)?.*"
    val replayRegexp  = ":r(e(p(l(a(y)?)?)?)?)?.*"

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
      out.println("Unknown command.  Type :help for help.")
    else
      shouldReplay = interpretStartingWith(line)

    (true, shouldReplay)
  }

  /** Interpret expressions starting with the first line.
    * Read lines until a complete compilation unit is available
    * or until a syntax error has been seen.  If a full unit is
    * read, go ahead and interpret it.  Return the full string
    * to be recorded for replay, if any.
    */
  def interpretStartingWith(code: String): Option[String] = {
    interpreter.interpret(code) match {
      case Interpreter.Success => Some(code)
      case Interpreter.Error => None
      case Interpreter.Incomplete =>
        if (in.interactive && code.endsWith("\n\n")) {
          out.println("You typed two blank lines.  Starting a new command.")
          None
        } else {
          val nextLine = in.readLine(continuationPrompt)
          if (nextLine == null)
            None  // end of file
          else
            interpretStartingWith(code + "\n" + nextLine)
        }
    }
  }
/*
  def loadFiles(settings: Settings) {
    settings match {
      case settings: GenericRunnerSettings =>
        for (filename <- settings.loadfiles.value) {
          val cmd = ":load " + filename
          command(cmd)
          replayCommandsRev = cmd :: replayCommandsRev
          out.println()
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
        repl(firstLine())
      }
    } finally {
      closeInterpreter()
    }
    ctx.reporter
  }
}
