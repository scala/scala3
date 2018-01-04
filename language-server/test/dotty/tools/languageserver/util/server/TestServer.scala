package dotty.tools.languageserver.util.server

import java.io.PrintWriter
import java.net.URI
import java.nio.file.Path
import java.util

import dotty.tools.languageserver.DottyLanguageServer
import org.eclipse.lsp4j._

import scala.collection.JavaConverters._

class TestServer(testFolder: Path) {

  // TODO initialise config file from DottyIDEPlugin
  private val baseDir = java.nio.file.Paths.get("..").toAbsolutePath
  private val ivyDir = java.nio.file.Paths.get("~/.ivy2").toAbsolutePath
  private val javaHome = System.getProperty("java.home")
  private val dottyIdeJson: String =
    s"""[ {
       |  "id" : "dotty-ide-test",
       |  "compilerVersion" : "0.6.0-bin-SNAPSHOT-nonbootstrapped",
       |  "compilerArguments" : [ "-feature", "-deprecation", "-unchecked", "-Xfatal-warnings", "-encoding", "UTF8", "-language:existentials,higherKinds,implicitConversions" ],
       |  "sourceDirectories" : [ "$baseDir/out/ide-tests/src" ],
       |  "dependencyClasspath" : [ "$baseDir/library/../out/bootstrap/dotty-library-bootstrapped/scala-0.7/classes", "$ivyDir/cache/org.scala-lang/scala-library/jars/scala-library-2.12.4.jar" ],
       |  "classDirectory" : "$baseDir/out/ide-tests/out"
       |}
       |]
    """.stripMargin
  private val configFile = testFolder.resolve(DottyLanguageServer.IDE_CONFIG_FILE)
  testFolder.toFile.mkdirs()
  testFolder.resolve("src").toFile.mkdirs()
  testFolder.resolve("out").toFile.mkdirs()
  new PrintWriter(configFile.toString) { write(dottyIdeJson); close() }

  val server = new DottyLanguageServer
  private val client = new TestClient
  server.connect(client)

  private val initParams = new InitializeParams()
  initParams.setRootUri(testFolder.toAbsolutePath.toUri.toString)
  server.initialize(initParams).get()

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
