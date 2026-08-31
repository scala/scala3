package dotty.tools.repl

import java.io.File
import java.net.{URL, URLClassLoader}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import dotty.tools.repl.AbstractFileClassLoader

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

        Right(fetch.fetch().asScala.toList)

      catch
        case NonFatal(e) =>
          Left(s"Failed to resolve dependencies: ${e.getMessage}")

  /** Add resolved dependencies to the compiler classpath and classloader.
   *  Returns the new classloader.
   *
   *  This follows the same pattern as the `:jar` command.
   */
  def addToCompilerClasspath(
    files: List[File],
    prevClassLoader: ClassLoader,
    prevOutputDir: dotty.tools.io.AbstractFile
  )(using ctx: dotty.tools.dotc.core.Contexts.Context): AbstractFileClassLoader =
    import dotty.tools.dotc.classpath.ClassPathFactory
    import dotty.tools.dotc.core.SymbolLoaders
    import dotty.tools.dotc.core.Symbols.defn
    import dotty.tools.io.{AbstractFile, ClassPath}
    import dotty.tools.repl.ScalaClassLoader.fromURLsParallelCapable

    // Create a classloader with all the resolved JAR files
    val urls = files.map(_.toURI.toURL).toArray
    val depsClassLoader = new URLClassLoader(urls, prevClassLoader)

    // Add each JAR to the compiler's classpath
    for file <- files do
      val jarFile = AbstractFile.getDirectory(file.getAbsolutePath, ctx.settings.javaOutputVersion.value)
      if jarFile != null then
        val jarClassPath = ClassPathFactory.newClassPath(jarFile)
        ctx.platform.addToClassPath(jarClassPath)
        SymbolLoaders.mergeNewEntries(defn.RootClass, ClassPath.RootPackage, jarClassPath, ctx.platform.classPath)

    // Create new classloader with previous output dir and resolved dependencies
    new AbstractFileClassLoader(
      prevOutputDir,
      depsClassLoader,
      AbstractFileClassLoader.InterruptInstrumentation.fromString(ctx.settings.XreplInterruptInstrumentation.value)
    )

end DependencyResolver
