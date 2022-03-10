package dotty.tools
package languageserver

import java.util.function.Consumer

import java.io.{ File => JFile, InputStream, OutputStream, PrintWriter }
import java.net._
import java.nio.channels._

import org.eclipse.lsp4j._
import org.eclipse.lsp4j.services._
import org.eclipse.lsp4j.launch._
import org.eclipse.lsp4j.jsonrpc.Launcher

/** Run the Dotty Language Server.
 */
object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case List("-stdio") =>
        val serverIn = System.in
        val serverOut = System.out
        System.setOut(System.err)
        scala.Console.withOut(scala.Console.err) {
          startServer(serverIn, serverOut)
        }
      case _ =>
        Console.err.println("Invalid arguments: expected \"-stdio\" or \"-client_command ...\"")
        System.exit(1)
    }
  }

  def startServer(in: InputStream, out: OutputStream) = {
    val server = new DottyLanguageServer

    println("Starting server")
    val launcher =
      new Launcher.Builder[DottyClient]()
        .setLocalService(server)
        .setRemoteInterface(classOf[DottyClient])
        .setInput(in)
        .setOutput(out)
        // For debugging JSON messages:
        //.traceMessages(new java.io.PrintWriter(System.err, true))
        .create();

    val client = launcher.getRemoteProxy()
    server.connect(client)
    launcher.startListening()
  }
}
