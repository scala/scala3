package dotty.tools.repl

import scala.language.unsafeNulls

import java.io.File
import java.net.{URL, URLClassLoader}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

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
    // Match either org:artifact:version or org::artifact:version
    val pattern = """([^:]+)::?([^:]+):([^:]+)""".r

    dep match
      case pattern(org, artifact, version) =>
        val isScalaStyle = dep.contains("::")
        val fullArtifact = if isScalaStyle then s"${artifact}_3" else artifact
        Some((org, fullArtifact, version))
      case _ => None

  /** Extract all dependencies from using directives in source code */
  def extractDependencies(sourceCode: String): List[String] =
    try
      val processor = new UsingDirectivesProcessor()
      val directives = processor.extract(sourceCode.toCharArray)

      val deps = scala.collection.mutable.ListBuffer[String]()

      directives.asScala.foreach { directive =>
        val flatMap = directive.getFlattenedMap
        flatMap.asScala.foreach { case (path, values) =>
          // Check if this is a "dep" directive (path segments: ["dep"])
          if path.getPath.asScala.toList == List("dep") then
            values.asScala.foreach { value =>
              value match
                case strValue: StringValue =>
                  deps += strValue.get()
                case _ =>
            }
        }
      }

      deps.toList
    catch
      case NonFatal(e) =>
        // If parsing fails, fall back to empty list
        Nil

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
        val deps = dependencies.map { case (org, artifact, version) =>
          Dependency.of(org, artifact, version)
        }.toArray

        val fetch = coursierapi.Fetch.create()
          .withRepositories(repos*)
          .withDependencies(deps*)

        val files = fetch.fetch().asScala.toList
        Right(files)

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
    import dotty.tools.io.*
    import dotty.tools.runner.ScalaClassLoader.fromURLsParallelCapable

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
    new AbstractFileClassLoader(prevOutputDir, depsClassLoader)

end DependencyResolver
