package dotty.tools.languageserver.util.server

import java.io.PrintWriter
import java.io.File.{separator => sep}
import java.net.URI
import java.nio.file.{Files, Path}
import java.util

import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.util.Code.Workspace
import org.eclipse.lsp4j.{ DidOpenTextDocumentParams, InitializeParams, InitializeResult, TextDocumentItem}

class TestServer(testFolder: Path, workspaces: List[Workspace]) {

  val server = new DottyLanguageServer
  var client: TestClient = _

  init()

  private[this] def init(): InitializeResult = {
    /**
     * Set up given workspace, return JSON config.
     *
     * This creates the necessary directories to hold the classes and sources. Some values
     * are passed via sbt-buildinfo, such as the classpath containing the scala and dotty libaries.
     *
     * @param workspace The workspace to configure.
     * @return A JSON object representing the configuration for this workspace.
     */
    def workspaceSetup(workspace: Workspace): String = {
      def showSeq[T](lst: Seq[T]): String =
        lst
          .map(elem => '"' + elem.toString.replace('\\', '/') + '"')
          .mkString("[ ", ", ", " ]")

      def classDirectory(workspace: Workspace): Path = {
        val path = testFolder.resolve(workspace.name).resolve("out")
        Files.createDirectories(path)
        path.toAbsolutePath
      }

      val dependencyClasspath =
        BuildInfo.ideTestsDependencyClasspath.map(_.getAbsolutePath) ++
          workspace.dependsOn.map(w => classDirectory(w).toString)

      val sourceDirectory: Path = {
        val path = TestFile.sourceDir.resolve(workspace.name).toAbsolutePath
        Files.createDirectories(path)
        path
      }

      s"""{
         |  "id" : "${workspace.name}",
         |  "compilerVersion" : "${BuildInfo.ideTestsCompilerVersion}",
         |  "compilerArguments" : ${showSeq(BuildInfo.ideTestsCompilerArguments)},
         |  "sourceDirectories" : ${showSeq(sourceDirectory :: Nil)},
         |  "dependencyClasspath" : ${showSeq(dependencyClasspath)},
         |  "classDirectory" : "${classDirectory(workspace).toString.replace('\\','/')}"
         |}
         |""".stripMargin
    }

    Files.createDirectories(testFolder)
    val configFile = testFolder.resolve(DottyLanguageServer.IDE_CONFIG_FILE)
    val configuration = workspaces.map(workspaceSetup).mkString("[", ",", "]")

    new PrintWriter(configFile.toString) {
      write(configuration)
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
  def openCode(code: String, workspace: Workspace, fileName: String): TestFile = {
    val testFile = new TestFile(workspace.name + sep + fileName)
    val dotdp = new DidOpenTextDocumentParams()
    val tdi = new TextDocumentItem()
    tdi.setUri(testFile.uri)
    tdi.setText(code)
    dotdp.setTextDocument(tdi)
    server.didOpen(dotdp)
    testFile
  }

}
