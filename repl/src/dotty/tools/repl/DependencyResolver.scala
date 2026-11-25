package dotty.tools.repl

import java.io.File
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import coursierapi.{Dependency, IvyRepository, MavenRepository, Repository}

/** Handles dependency resolution using Coursier for the REPL */
object DependencyResolver:

  private val defaultRepositories: List[Repository] = List(
    MavenRepository.of("https://repo1.maven.org/maven2"),
  )

  // TODO: support every alias coursier does, once the Coursier Interface exposes its own
  // `parseRepository` and parsing can be delegated to it.
  private val repositoryAliases: Map[String, Repository] =
    val m2Local = MavenRepository.of(File(sys.props("user.home"), ".m2/repository").toURI.toString)
    Map(
      "central" -> Repository.central(),
      "ivy2Local" -> Repository.ivy2Local(),
      "ivy2local" -> Repository.ivy2Local(),
      "m2Local" -> m2Local,
      "m2local" -> m2Local
    )

  /** Parse a repository given as one of the [[repositoryAliases]], an `ivy:<pattern>`, a URL or a
   *  local directory.
   */
  def parseRepository(repository: String): Option[Repository] =
    repositoryAliases.get(repository).orElse:
      repository match
        case s"ivy:$pattern" if pattern.nonEmpty =>
          pattern.split("\\|", 2) match
            case Array(artifacts, metadata) => Some(IvyRepository.of(artifacts, metadata))
            case _ => Some(IvyRepository.of(pattern))
        case url if url.contains("://") => Some(MavenRepository.of(url))
        case path if path.contains(File.separatorChar) && File(path).isDirectory =>
          Some(MavenRepository.of(File(path).toURI.toString))
        case _ => None


  /** Parse a dependency string of the form `org::artifact:version` or `org:artifact:version`
   *  and return the (organization, artifact, version) triple if successful.
   *
   *  Supports both Maven-style (single colon) and Scala-style (double colon) notation:
   *  - Maven: `com.lihaoyi:scalatags_3:0.13.1`
   *  - Scala: `com.lihaoyi::scalatags:0.13.1` (automatically appends _3)
   */
  def parseDependency(dep: String): Option[(String, String, String)] =
    dep match
      case s"$org::$artifact:$version" => Some((org, s"${artifact}_3", version))
      case s"$org:$artifact:$version" => Some((org, artifact, version))
      case _ =>
        System.err.println("Unable to parse dependency \"" + dep + "\"")
        None

  /** Resolve dependencies using Coursier Interface and return the classpath as a list of File objects */
  def resolveDependencies(
    dependencies: List[(String, String, String)],
    repositories: List[Repository] = Nil
  ): Either[String, List[File]] =
    if dependencies.isEmpty then Right(Nil)
    else
      try
        val repos = (repositories ++ defaultRepositories).toArray

        // Create dependency objects
        val deps = dependencies
          .map { case (org, artifact, version) => Dependency.of(org, artifact, version) }
          .toArray

        val fetch = coursierapi.Fetch.create()
          .withRepositories(repos*)
          .withDependencies(deps*)

        Right(withUserAgent(fetch.fetch().asScala.toList))

      catch
        case NonFatal(e) =>
          Left(s"Failed to resolve dependencies: ${e.getMessage}")

  private val userAgentProperty = "coursier.http.agent"

  /** The HTTP User-Agent identifying the REPL to repositories, as Maven Central rate-limits
   *  generic ones (see https://central.sonatype.org/faq/429-error/).
   */
  private def userAgent: String =
    val version = Option(dotty.tools.dotc.config.Properties.versionNumberString).filter(_.nonEmpty).getOrElse("unknown")
    s"Scala-REPL/$version (+https://github.com/scala/scala3) Coursier"

  /** Run `body` with coursier identifying itself with [[userAgent]], unless a user agent
   *  was explicitly set with the `coursier.http.agent` system property.
   *
   *  The Coursier Interface does not expose a way to set the user agent, so the system property
   *  is set for the duration of `body`, and then restored, not to leak into the REPL session.
   */
  private def withUserAgent[T](body: => T): T =
    if sys.props.contains(userAgentProperty) then body
    else
      sys.props(userAgentProperty) = userAgent
      try body
      finally sys.props.remove(userAgentProperty)

  /** Add resolved dependencies to the compiler classpath and classloader.
   *
   *  This follows the same pattern as the `:jar` command.
   */
  def addToCompilerClasspath(files: List[File])(using ctx: dotty.tools.dotc.core.Contexts.Context): Unit =
    import dotty.tools.dotc.classpath.{ClassPath, ClassPathFactory}
    import dotty.tools.dotc.core.SymbolLoaders
    import dotty.tools.dotc.core.Symbols.defn
    import dotty.tools.io.AbstractFile

    for file <- files do
      val jarFile = AbstractFile.getDirectory(file.getAbsolutePath, ctx.settings.javaOutputVersion.value)
      if jarFile != null then
        val jarClassPath = ClassPathFactory.newClassPath(jarFile)
        ctx.platform.addToClassPath(jarClassPath)
        SymbolLoaders.mergeNewEntries(defn.RootClass, ClassPath.RootPackage, jarClassPath, ctx.platform.classPath)

end DependencyResolver
