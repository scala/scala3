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
 *
 *  This is designed to be started from an editor supporting the Language Server
 *  Protocol, the easiest way to fetch and run this is to use `coursier`:
 *
 *    coursier launch $artifact -M dotty.tools.languageserver.Main -- -stdio
 *
 *  Where $artifact comes from the `.dotty-ide-artifact` file in the current project, this file
 *  can be created by the DottyIDEPlugin by running `sbt configureIDE`.
 *
 *  See vscode-dotty/ for an example integration of the Dotty Language Server into Visual Studio Code.
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
      case "-client_command" :: clientCommand =>
        val serverSocket = new ServerSocket(0)
        Runtime.getRuntime().addShutdownHook(new Thread(
          new Runnable {
            def run: Unit = {
              serverSocket.close()
            }
          }));

        println("Starting client: " + clientCommand)
        val clientPB = new java.lang.ProcessBuilder(clientCommand: _*)
        clientPB.environment.put("DLS_DEV_MODE", "1")

        val pw = new PrintWriter("../.dotty-ide-dev-port")
        pw.write(serverSocket.getLocalPort.toString)
        pw.close()

        clientPB.inheritIO().start()

        val clientSocket = serverSocket.accept()

        startServer(clientSocket.getInputStream, clientSocket.getOutputStream)
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
