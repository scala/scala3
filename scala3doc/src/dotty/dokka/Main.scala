package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import scala.tasty.inspector.TastyInspector
import java.nio.file.Files

import dotty.tools.dotc.config.Settings._

abstract class Scala3Args extends SettingGroup:
  val tastyRoots: Setting[String] =
    StringSetting("--tastyRoots", "tastyRoots", "Roots where tools should look for tasty files", "", aliases = List("-t"))
  val tastyRootsAlias: Setting[String] =
    StringSetting("-t", "tastyRoots", "Roots where tools should look for tasty files", "", aliases = List("-t"))

  val dest: Setting[String] =
    StringSetting("--dest", "dest", "Output to generate documentation to", "", aliases = List("-d"))
  val destAlias: Setting[String] =
    StringSetting("-d", "dest", "Output to generate documentation to", "")

  val classpath: Setting[String] =
    StringSetting("--classpath", "classpath", "Classpath to load dependencies from", "", aliases = List("--cp", "-c"))
  val classpathAlias1: Setting[String] =
    StringSetting("--cp", "classpath", "Classpath to load dependencies from", "", aliases = List("--cp", "-c"))
  val classpathAlias2: Setting[String] =
    StringSetting("-c", "classpath", "Classpath to load dependencies from", "", aliases = List("--cp", "-c"))

  val name: Setting[String] =
    StringSetting("--name", "name", "Name of module in generated documentation", "", aliases = List("-n"))
  val nameAlias: Setting[String] =
    StringSetting("-n", "name", "Name of module in generated documentation", "")

  val docsRoot: Setting[String] =
    StringSetting("--docs", "docs", "Root of project docs", "", aliases = List("-p"))
  val docsRootAlias: Setting[String] =
    StringSetting("-p", "docs", "Root of project docs", "", aliases = List("-p"))


  val sourceLinks: Setting[String] =
    StringSetting("--sources", "sources", "Links to source files provided in convention: local_directory=remote_directory#line_suffix", "")
  val sourceLinksAlias: Setting[String] =
    StringSetting("-s", "sources", "Links to source files provided in convention: local_directory=remote_directory#line_suffix", "")

  val projectTitle: Setting[String] =
    StringSetting("--projectTitle", "projectTitle", "Title of the project used in documentation", "")

  val projectVersion: Setting[String] =
    StringSetting("--projectVersion", "projectVersion", "Version of the project used in documentation", "")

  val projectLogo: Setting[String] =
    StringSetting("--projectLogo", "projectLogo", "Relative path to logo of the project", "")

  val revision: Setting[String] =
    StringSetting("--revision", "revision", "Revision (branch or ref) used to build project project", "")

  val syntax: Setting[String] =
    StringSetting("--syntax", "syntax", "Syntax of the comment used", "")

  protected def defaultName(): String
  protected def defaultTastFiles(): List[File]
  protected def defaultDest(): File

  def extract(args: List[String]) =
    val initialSummary = ArgsSummary(defaultState, args, errors = Nil, warnings = Nil)
    val res = processArguments(initialSummary, processAll = true, skipped = Nil)
    // TODO!
    if res.errors.nonEmpty then sys.error(s"Unable to parse arguments:\n ${res.errors.mkString("\n")}")

    val parsedSyntax = syntax.valueIn(res.sstate) match
      case "" => None
      case other =>
        Args.CommentSyntax.fromString(other) match
          case None =>
            sys.error(s"unrecognized value for --syntax option: $other")
          case some => some

    def parseOptionalArg(args: Setting[String]*) =
      args.map(_.valueIn(res.sstate)).find(_ != "")

    def parseTastyRoots(roots: String) = roots.split(File.pathSeparatorChar).toList.map(new File(_))

    Args(
      parseOptionalArg(name, nameAlias).getOrElse(defaultName()),
      parseOptionalArg(tastyRoots, tastyRootsAlias).fold(defaultTastFiles())(parseTastyRoots),
      parseOptionalArg(classpath, classpathAlias1, classpathAlias2).getOrElse(System.getProperty("java.class.path")),
      parseOptionalArg(dest, destAlias).fold(defaultDest())(new File(_)),
      parseOptionalArg(docsRoot, docsRootAlias),
      parseOptionalArg(projectVersion),
      parseOptionalArg(projectTitle),
      parseOptionalArg(projectLogo),
      parsedSyntax,
      parseOptionalArg(sourceLinks, sourceLinksAlias).fold(Nil)(_.split(",").toList), // TODO!
      parseOptionalArg(revision)
    )

case class Args(
  name: String,
  tastyRoots: Seq[File],
  classpath: String,
  output: File,
  docsRoot: Option[String] = None,
  projectVersion: Option[String] = None,
  projectTitle: Option[String] = None,
  projectLogo: Option[String] = None,
  defaultSyntax: Option[Args.CommentSyntax] = None,
  sourceLinks: List[String] = Nil,
  revision: Option[String] = None
)

object Args:
  enum CommentSyntax:
    case Wiki
    case Markdown

  object CommentSyntax:
    def fromString(str: String): Option[CommentSyntax] =
      str match
        case "wiki" => Some(Wiki)
        case "markdown" => Some(Markdown)
        case _ => None
end Args

import dotty.tools.dotc.core.Contexts.{Context => DottyContext}
trait BaseDocConfiguration:
  val args: Args
  val tastyFiles: List[String]

enum DocConfiguration extends BaseDocConfiguration:
  case Standalone(args: Args, tastyFiles: List[String], tastyJars: List[String])
  case Sbt(args: Args, tastyFiles: List[String], rootCtx: DottyContext)

/** Main class for the doctool.
  *
  * The `main` method is mostly responsible just for parsing arguments and
  * configuring Dokka. After that, we hand control to Dokka.
  *
  * Other important classes:
  *
  * - [](package.DottyDokkaPlugin) is our class that Dokka calls back and which
  *   actually generates the documentation.
  * - [](package.DottyDokkaConfig) is our config for Dokka.
  */
object Main:
  def main(parsedArgs: Args): Unit =
    try
      val (files, dirs) = parsedArgs.tastyRoots.partition(_.isFile)
      val (providedTastyFiles, jars) = files.toList.map(_.getAbsolutePath).partition(_.endsWith(".tasty"))
      jars.foreach(j => if(!j.endsWith(".jar")) sys.error(s"Provided file $j is not jar not tasty file") )


      def listTastyFiles(f: File): Seq[String] =
        val (files, dirs) = f.listFiles().partition(_.isFile)
        ArraySeq.unsafeWrapArray(
          files.filter(_.getName.endsWith(".tasty")).map(_.toString) ++ dirs.flatMap(listTastyFiles)
        )
      val tastyFiles = providedTastyFiles ++ dirs.flatMap(listTastyFiles)

      val config = DocConfiguration.Standalone(parsedArgs, tastyFiles, jars)

      if (parsedArgs.output.exists()) IO.delete(parsedArgs.output)

      // TODO #20 pass options, classpath etc.
      new DokkaGenerator(new DottyDokkaConfig(config), DokkaConsoleLogger.INSTANCE).generate()

      println("Done")
    catch
      case a: Exception =>
        a.printStackTrace()
        // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
        sys.exit(1)

  def main(args: Array[String]): Unit =
      val argDefinition = new Scala3Args {
        protected def defaultName(): String = sys.error(s"Argument '${name.name}' is required")
        protected def defaultTastFiles(): List[File] = sys.error(s"Argument '${tastyRoots.name}' is required")
        protected def defaultDest(): File = sys.error(s"Argument '${dest.name}' is required")
      }
      main(argDefinition.extract(args.toList))
      // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
      sys.exit(0)


