package dotty.tools.languageserver.util.server

import java.io.PrintWriter
import java.io.File.{pathSeparator, separator}
import java.net.URI
import java.nio.file.{Files, Path}
import java.util

import dotty.tools.dotc.Main
import dotty.tools.dotc.reporting.{Reporter, ThrowingReporter}
import dotty.tools.io.Directory
import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.util.Code.{TastyWithPositions, Workspace}
import org.eclipse.lsp4j.{ DidOpenTextDocumentParams, InitializeParams, InitializeResult, TextDocumentItem}

class TestServer(testFolder: Path, workspaces: List[Workspace]) {

  val server = new DottyLanguageServer
  var client: TestClient = _

  init()

  private[this] def init(): InitializeResult = {
    var compiledWorkspaces: Set[Workspace] = Set.empty

    /** Compile the dependencies of the given workspace, and then the workspace. */
    def compileWorkspaceAndDependencies(workspace: Workspace): Unit =
      if (!compiledWorkspaces.contains(workspace)) {
        workspace.dependsOn.foreach(compileWorkspaceAndDependencies)
        compileWorkspace(workspace)
        compiledWorkspaces += workspace
      }

    /**
     * Set up given workspace, return JSON config.
     *
     * If the workspace has dependencies, these dependencies are compiled. The classfiles of the
     * dependent workspaces are put on the classpath of this workspace.
     *
     * @param workspace The workspace to configure.
     * @return A JSON object representing the configuration for this workspace.
     */
    def workspaceSetup(workspace: Workspace): String = {
      def showSeq[T](lst: Seq[T]): String =
        lst
          .map(elem => '"' + elem.toString.replace('\\', '/') + '"')
          .mkString("[ ", ", ", " ]")

      if (workspace.sources.exists(_.isInstanceOf[TastyWithPositions])) {
        compileWorkspaceAndDependencies(workspace)
      } else {
        // Compile all the dependencies of this workspace
        workspace.dependsOn.foreach(compileWorkspaceAndDependencies)
      }

      s"""{
         |  "id" : "${workspace.name}",
         |  "compilerVersion" : "${BuildInfo.ideTestsCompilerVersion}",
         |  "compilerArguments" : ${showSeq(BuildInfo.ideTestsCompilerArguments)},
         |  "sourceDirectories" : ${showSeq(sourceDirectory(workspace, wipe = false) :: Nil)},
         |  "dependencyClasspath" : ${showSeq(dependencyClasspath(workspace))},
         |  "classDirectory" : "${classDirectory(workspace, wipe = false).toString.replace('\\','/')}"
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
   *  @param openInIDE If true, send `textDocument/didOpen` to the server.
   *  @return the file opened
   */
  def openCode(code: String, workspace: Workspace, fileName: String, openInIDE: Boolean): TestFile = {
    val testFile = new TestFile(workspace.name + separator + fileName)
    val tdi = new TextDocumentItem()
    tdi.setUri(testFile.uri)
    tdi.setText(code)

    if (openInIDE) {
      val dotdp = new DidOpenTextDocumentParams()
      dotdp.setTextDocument(tdi)
      server.didOpen(dotdp)
    }

    testFile
  }

  private def classDirectory(workspace: Workspace, wipe: Boolean): Path = {
    val path = testFolder.resolve(workspace.name).resolve("out")
    if (wipe) {
      Directory(path).deleteRecursively()
      Files.createDirectories(path)
    }
    path.toAbsolutePath
  }

  private def dependencyClasspath(workspace: Workspace): Seq[String] = {
    BuildInfo.ideTestsDependencyClasspath.map(_.getAbsolutePath) ++
      workspace.dependsOn.flatMap { dep =>
        classDirectory(dep, wipe = false).toString +: dependencyClasspath(dep)
      }
  }.distinct

  private def sourceDirectory(workspace: Workspace, wipe: Boolean): Path = {
    val path = TestFile.sourceDir.resolve(workspace.name).toAbsolutePath
    if (wipe) {
      Directory(path).deleteRecursively()
      Files.createDirectories(path)
    }
    path
  }

  /**
   * Sets up the sources of the given workspace, creates the necessary directories
   * and compile the sources.
   *
   * @param workspace The workspace to set up.
   */
  private def compileWorkspace(workspace: Workspace): Unit = {
    val sourcesDir = sourceDirectory(workspace, wipe = true)
    val sources = workspace.sources.zipWithIndex.map { case (src, id) =>
      val path = sourcesDir.resolve(src.sourceName(id)).toAbsolutePath
      Files.write(path, src.text.getBytes("UTF-8"))
      path.toString
    }

    val compileOptions =
      sources.toArray ++
        Array(
          "-classpath", dependencyClasspath(workspace).mkString(pathSeparator),
          "-d", classDirectory(workspace, wipe = true).toString
        )
    val reporter = new ThrowingReporter(Reporter.NoReporter)
    Main.process(compileOptions, reporter)
  }

}
