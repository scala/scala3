package dotty.tools.languageserver.util.server

import java.io.PrintWriter
import java.net.URI
import java.nio.file.Path
import java.util

import dotty.tools.languageserver.DottyLanguageServer
import org.eclipse.lsp4j.{ DidOpenTextDocumentParams, InitializeParams, InitializeResult, TextDocumentItem}

class TestServer(testFolder: Path) {

  val server = new DottyLanguageServer
  var client: TestClient = _

  init()

  private[this] def init(): InitializeResult = {
    // Fill the configuration with values populated by sbt
    def showSeq[T](lst: Seq[T]): String =
      lst
        .map(elem => '"' + elem.toString.replace('\\', '/') + '"')
        .mkString("[ ", ", ", " ]")
    val dottyIdeJson: String =
      s"""[ {
         |  "id" : "dotty-ide-test",
         |  "compilerVersion" : "${BuildInfo.ideTestsCompilerVersion}",
         |  "compilerArguments" : ${showSeq(BuildInfo.ideTestsCompilerArguments)},
         |  "sourceDirectories" : ${showSeq(BuildInfo.ideTestsSourceDirectories)},
         |  "dependencyClasspath" : ${showSeq(BuildInfo.ideTestsDependencyClasspath)},
         |  "classDirectory" : "${BuildInfo.ideTestsClassDirectory.toString.replace('\\','/')}"
         |}
         |]""".stripMargin
    val configFile = testFolder.resolve(DottyLanguageServer.IDE_CONFIG_FILE)
    testFolder.toFile.mkdirs()
    testFolder.resolve("src").toFile.mkdirs()
    testFolder.resolve("out").toFile.mkdirs()

    new PrintWriter(configFile.toString) {
      write(dottyIdeJson)
      close()
    }

    client = new TestClient
    server.connect(client)

    val initParams = new InitializeParams()
    initParams.setRootUri(testFolder.toAbsolutePath.toUri.toString)
    server.initialize(initParams).get()
  }

  /** Open the code in the given file and returns the file.
   *  @param code code in file
   *  @param fileName file path in the source directory
   *  @return the file opened
   */
  def openCode(code: String, fileName: String): TestFile = {
    val testFile = new TestFile(fileName)
    val dotdp = new DidOpenTextDocumentParams()
    val tdi = new TextDocumentItem()
    tdi.setUri(testFile.uri)
    tdi.setText(code)
    dotdp.setTextDocument(tdi)
    server.didOpen(dotdp)
    testFile
  }

}
