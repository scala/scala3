package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.utilities._
import org.jetbrains.dokka.plugability._
import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import java.util.{List => JList}

import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import sbt.io.IO
import java.nio.file.Files

import org.kohsuke.args4j.{CmdLineParser, Option => COption}
import org.kohsuke.args4j.spi.StringArrayOptionHandler

class RawArgs:
    @COption(name="--tastyRoots", required = true, aliases = Array("-t"), usage="Roots where tools should look for tasty files")
    private var tastyRoots: String = null

    @COption(name="--output",required = true, aliases = Array("-o"), usage="Output to generate documentation to")
    private var output: String = "output"

    @COption(name="--classpath", aliases = Array("--cp", "-c"), usage="Classpath to load dependencies from")
    private var classpath: String = System.getProperty("java.class.path")

    @COption(name="--name", required = true, aliases = Array("-n"), usage="Name of module in generated documentation")
    private var name: String = "main"

    @COption(name="--docs", aliases = Array("-d"), usage="Root of project docs")
    private var docsRoot: String =  null

    @COption(name="--sources", handler = classOf[StringArrayOptionHandler], aliases = Array("-s"), usage = "Links to source files provided in convention: local_directory=remote_directory#line_suffix")
    private var sourceLinks: JList[String] = null
    
    @COption(name="--projectTitle")
    private var projectTitle: String = null

    @COption(name="--projectVersion")
    private var projectVersion: String = null

    @COption(name="--projectLogo")
    private var projectLogo: String = null

    @COption(name="--syntax")
    private var syntax: String = null

    def toArgs =
      val parsedSyntax = syntax match
        case null => None
        case other =>
          Args.CommentSyntax.fromString(other) match
            case None =>
              sys.error(s"unrecognized value for --syntax option: $other")
            case some => some

      Args(
        name,
        tastyRoots.split(File.pathSeparatorChar).toList.map(new File(_)),
        classpath,
        new File(output),
        Option(docsRoot),
        projectVersion,
        Option(projectTitle),
        Option(projectLogo),
        parsedSyntax,
        Option(sourceLinks).map(_.asScala.toList).getOrElse(List.empty)
      )


case class Args(
  name: String,
  tastyRoots: Seq[File],
  classpath: String,
  output: File,
  docsRoot: Option[String],
  projectVersion: String,
  projectTitle: Option[String],
  projectLogo: Option[String],
  defaultSyntax: Option[Args.CommentSyntax],
  sourceLinks: List[String]
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

case class DocConfiguration(tastyFiles: List[String], args: Args)

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
  def main(args: Array[String]): Unit = 
    try 
      val rawArgs = new RawArgs
      new CmdLineParser(rawArgs).parseArgument(args:_*)
      val parsedArgs = rawArgs.toArgs

      val (jars, dirs) = parsedArgs.tastyRoots.partition(_.isFile)
      val extracted = jars.filter(_.exists()).map { jarFile =>
          val tempFile = Files.createTempDirectory("jar-unzipped").toFile
          IO.unzip(jarFile, tempFile)
          tempFile
      }

      try 
        def listTastyFiles(f: File): Seq[String] =
          val (files, dirs) = f.listFiles().partition(_.isFile)
          files.filter(_.getName.endsWith(".tasty")).map(_.toString) ++ dirs.flatMap(listTastyFiles)
        
        val tastyFiles = (dirs ++ extracted).flatMap(listTastyFiles).toList

        val config = DocConfiguration(tastyFiles, parsedArgs)

        if (parsedArgs.output.exists()) IO.delete(parsedArgs.output)

        // TODO #20 pass options, classpath etc.
        new DokkaGenerator(new DottyDokkaConfig(config), DokkaConsoleLogger.INSTANCE).generate()

        println("Done")
        
      
      finally
        extracted.foreach(IO.delete)
      // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
      sys.exit(0)
    catch 
      case a: Exception =>
        a.printStackTrace()
        // Sometimes jvm is hanging, so we want to be sure that we force shout down the jvm
        sys.exit(1)     
