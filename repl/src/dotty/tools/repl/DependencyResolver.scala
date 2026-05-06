package dotty.tools.repl

import scala.language.unsafeNulls

import java.io.File
import java.net.{URL, URLClassLoader}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import dotty.tools.repl.AbstractFileClassLoader

import coursierapi.{Repository, Dependency, MavenRepository}
import com.virtuslab.using_directives.UsingDirectivesProcessor
import com.virtuslab.using_directives.custom.model.{Path, StringValue, Value}

/** Handles dependency resolution using Coursier for the REPL */
object DependencyResolver:

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

  /** Extract all dependencies from using directives in source code */
  def extractDependencies(sourceCode: String): List[String] =
    try
      val directives = new UsingDirectivesProcessor().extract(sourceCode.toCharArray)
      val deps = scala.collection.mutable.Buffer[String]()

      for
        directive <- directives.asScala
        (path, values) <- directive.getFlattenedMap.asScala
      do
        if path.getPath.asScala.toList == List("dep") then
          values.asScala.foreach {
            case strValue: StringValue => deps += strValue.get()
            case value => System.err.println("Unrecognized directive value " + value)
          }
        else
          System.err.println("Unrecognized directive " + path.getPath)

      deps.toList
    catch
      case NonFatal(e) => Nil // If parsing fails, fall back to empty list

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
      val jarFile = AbstractFile.getDirectory(file.getAbsolutePath)
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
