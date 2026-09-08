package dotty.tools.repl.worksheet

import dotty.tools.repl.DependencyResolver
import dotty.tools.repl.ReplDirectives
import dotty.tools.repl.ReplDirectives.DirectiveLines
import dotty.tools.repl.ReplDirectives.ReplDirective
import dotty.tools.repl.State

import java.io.File

private final case class DirectiveOutcome(
    dependencies: List[WorksheetDependency],
    repositories: List[String],
    classpath: List[File],
    diagnostics: List[WorksheetDiagnostic],
    state: State
)

private object WorksheetDependencies:
  private def message(warning: ReplDirectives.Warning): String = warning match
    case ReplDirectives.Warning.UnsupportedDirective(key) =>
      s"The `using $key` directive is not supported in worksheets."
    case other => other.toString.stripPrefix("[warn] ")

  def resolve(
      declared: DirectiveLines,
      text: String,
      state: State
  ): DirectiveOutcome =
    if declared.lines.isEmpty then DirectiveOutcome(Nil, Nil, Nil, Nil, state)
    else
      def diagnostic(line: Int, message: String, severity: WorksheetDiagnosticSeverity) =
        WorksheetDiagnostic(WorksheetSession.lineRange(text, line), message, severity)

      def error(line: Int, message: String) =
        diagnostic(line, message, WorksheetDiagnosticSeverity.Error)

      def valuesOf[A](values: PartialFunction[ReplDirective, A]): List[(Int, A)] =
        declared.lines.flatMap: line =>
          line.directives.collect(values).map(line.number -> _)

      val repositoryStrings = valuesOf { case ReplDirective.Repository(repository) => repository }
      val (badRepositories, repositories) =
        repositoryStrings.partitionMap: (line, repository) =>
          DependencyResolver.parseRepository(repository).toRight(line -> repository)

      val coordinates = valuesOf { case ReplDirective.Dependency(coordinate) => coordinate }
      val (unparsed, parsed) = coordinates.partitionMap: (line, coordinate) =>
        DependencyResolver.parseDependency(coordinate).toRight(line -> coordinate)

      val (missingJars, jars) = valuesOf { case ReplDirective.Jar(path) => path }
        .partitionMap: (line, path) =>
          val file = new File(path)
          if file.exists then Right(file) else Left(line -> path)

      val resolution =
        if parsed.isEmpty then Right(Nil)
        else DependencyResolver.resolveDependencies(parsed, repositories)

      val resolutionDiagnostics = resolution match
        case Right(_) => Nil
        case Left(failure) =>
          val line = coordinates.map((line, _) => line).minOption.getOrElse(0)
          error(line, s"Unable to resolve dependencies: $failure") :: Nil

      val resolved = resolution match
        case Right(_) =>
          parsed.map((organization, moduleName, version) =>
            WorksheetDependency(organization, moduleName, version)
          )
        case Left(_) => Nil

      DirectiveOutcome(
        resolved,
        repositoryStrings.map((_, repository) => repository),
        resolution.getOrElse(Nil) ::: jars,
        declared.lines.flatMap(line =>
          line.warnings.map(warning =>
            diagnostic(line.number, message(warning), WorksheetDiagnosticSeverity.Warning)
          )
        )
          ::: unparsed.map((line, coordinate) =>
            error(line, s"Unable to parse the dependency `$coordinate`.")
          )
          ::: badRepositories.map((line, repository) =>
            error(line, s"Unable to parse the repository `$repository`.")
          )
          ::: missingJars.map((line, path) => error(line, s"The jar `$path` does not exist."))
          ::: resolutionDiagnostics,
        state.copy(repositories = (state.repositories ::: repositories).distinct)
      )
