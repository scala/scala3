package dotty.tools.languageserver.util.server

import java.io.PrintWriter
import java.io.File.{pathSeparator, separator}
import java.net.URI
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import java.util

import scala.compiletime.uninitialized

import dotty.tools.dotc.Main
import dotty.tools.dotc.reporting.{Reporter, ThrowingReporter}
import dotty.tools.io.Directory
import dotty.tools.languageserver.DottyLanguageServer
import dotty.tools.languageserver.util.Code.{TastyWithPositions, Project}
import org.eclipse.lsp4j.{ DidOpenTextDocumentParams, InitializeParams, InitializeResult, TextDocumentItem}

class TestServer(testFolder: Path, projects: List[Project]) {

  val server = new DottyLanguageServer
  var client: TestClient = uninitialized

  init()

  private def init(): InitializeResult = {
    var compiledProjects: Set[Project] = Set.empty

    /** Compile the dependencies of the given project, and then the project. */
    def compileProjectAndDependencies(project: Project): Unit =
      if (!compiledProjects.contains(project)) {
        project.dependsOn.foreach(compileProjectAndDependencies)
        compileProject(project)
        compiledProjects += project
      }

    /**
     * Set up given project, return JSON config.
     *
     * If the project has dependencies, these dependencies are compiled. The classfiles of the
     * dependent projects are put on the classpath of this project.
     *
     * @param project The project to configure.
     * @return A JSON object representing the configuration for this project.
     */
    def projectSetup(project: Project): String = {
      def showSeq[T](lst: collection.Seq[T]): String =
        lst
          .map(elem => '"'.toString + elem.toString.replace('\\', '/') + '"'.toString)
          .mkString("[ ", ", ", " ]")

      if (project.sources.exists(_.isInstanceOf[TastyWithPositions])) {
        compileProjectAndDependencies(project)
      } else {
        // Compile all the dependencies of this project
        project.dependsOn.foreach(compileProjectAndDependencies)
      }

      s"""{
         |  "id" : "${project.name}",
         |  "compilerVersion" : "${BuildInfo.ideTestsCompilerVersion}",
         |  "compilerArguments" : ${showSeq(BuildInfo.ideTestsCompilerArguments)},
         |  "sourceDirectories" : ${showSeq(sourceDirectory(project, wipe = false) :: Nil)},
         |  "dependencyClasspath" : ${showSeq(dependencyClasspath(project))},
         |  "classDirectory" : "${classDirectory(project, wipe = false).toString.replace('\\','/')}",
         |  "projectDependencies": ${showSeq(project.dependsOn.map(_.name))}
         |}
         |""".stripMargin
    }

    Files.createDirectories(testFolder)
    val configFile = testFolder.resolve(DottyLanguageServer.IDE_CONFIG_FILE)
    val configuration = projects.map(projectSetup).mkString("[", ",", "]")

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
  def openCode(code: String, project: Project, fileName: String, openInIDE: Boolean): TestFile = {
    val testFile = new TestFile(project.name + separator + fileName)
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

  private def classDirectory(project: Project, wipe: Boolean): Path = {
    val path = testFolder.resolve(project.name).resolve("out")
    if (wipe) {
      Directory(path).deleteRecursively()
    }
    Files.createDirectories(path)
    path.toAbsolutePath
  }

  private def dependencyClasspath(project: Project): Seq[String] = {
    BuildInfo.ideTestsDependencyClasspath.map(_.getAbsolutePath) ++
      project.dependsOn.flatMap { dep =>
        classDirectory(dep, wipe = false).toString +: dependencyClasspath(dep)
      }
  }.distinct.toSeq

  private def sourceDirectory(project: Project, wipe: Boolean): Path = {
    val path = TestFile.sourceDir.resolve(project.name).toAbsolutePath
    if (wipe) {
      Directory(path).deleteRecursively()
      Files.createDirectories(path)
    }
    path
  }

  /**
   * Sets up the sources of the given project, creates the necessary directories
   * and compile the sources.
   *
   * @param project The project to set up.
   */
  private def compileProject(project: Project): Unit = {
    val sourcesDir = sourceDirectory(project, wipe = true)
    val sources = project.sources.zipWithIndex.map { case (src, id) =>
      val path = sourcesDir.resolve(src.sourceName(id)).toAbsolutePath
      Files.write(path, src.text.getBytes(StandardCharsets.UTF_8.name))
      path.toString
    }

    val compileOptions =
      sources.toArray ++
        Array(
          "-classpath", dependencyClasspath(project).mkString(pathSeparator),
          "-d", classDirectory(project, wipe = true).toString
        )
    val reporter = new ThrowingReporter(Reporter.NoReporter)
    Main.process(compileOptions, reporter)
  }

}
