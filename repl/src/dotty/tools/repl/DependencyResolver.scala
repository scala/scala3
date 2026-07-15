package dotty.tools.repl

import scala.language.unsafeNulls

import java.io.File
import java.net.{URL, URLClassLoader}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import dotty.tools.repl.AbstractFileClassLoader

import coursierapi.{Dependency, MavenRepository}
import dotty.tools.directives.{DirectiveValue, UsingDirectivesParser}

/** Handles dependency resolution using Coursier for the REPL */
object DependencyResolver:

  /** Result of classifying `//> using` directives in REPL input. */
  case class ClassifiedDirectives(deps: List[String], unsupportedKeys: List[String], hasDirectives: Boolean)

  /** Directive keys the REPL knows how to handle. Extend as more directives gain REPL support. */
  val supportedDirectives: Set[String] = Set("dep")

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

  /** Classify `//> using` directives in REPL input into dependency coordinates and unsupported keys. */
  def classifyDirectives(sourceCode: String): ClassifiedDirectives =
    try
      val result = UsingDirectivesParser.parse(sourceCode)
      val (supported, unsupported) = result.directives.partition(d => supportedDirectives.contains(d.key))
      val deps =
        supported
          .flatMap(_.values.collect { case DirectiveValue.StringVal(value, _, _) => value })
          .toList
      val unsupportedKeys = unsupported.map(_.key).distinct.toList
      ClassifiedDirectives(deps, unsupportedKeys, result.directives.nonEmpty)
    catch
      case NonFatal(_) => ClassifiedDirectives(Nil, Nil, false)

  /** Resolve dependencies using Coursier Interface and return the classpath as a list of File objects */
  def resolveDependencies(dependencies: List[(String, String, String)]): Either[String, List[File]] =
    if dependencies.isEmpty then Right(Nil)
    else
      try
        // Add Maven Central and Sonatype repositories
        val repos = Array(
          MavenRepository.of("https://repo1.maven.org/maven2"),
          MavenRepository.of("https://oss.sonatype.org/content/repositories/releases")
        )

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
