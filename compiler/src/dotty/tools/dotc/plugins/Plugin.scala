package dotty.tools.dotc
package plugins

import core._
import Contexts._
import Phases._
import dotty.tools.io._
import transform.MegaPhase.MiniPhase

import java.io.InputStream
import java.util.Properties

import scala.util.{ Try, Success, Failure }

trait PluginPhase extends MiniPhase {
  def runsBefore: Set[String] = Set.empty
}

sealed trait Plugin {
  /** The name of this plugin */
  def name: String

  /** A one-line description of the plugin */
  def description: String

  /** Is this plugin a research plugin?
   *
   *  Research plugin receives a phase plan and return a new phase plan, while
   *  non-research plugin returns a list of phases to be inserted.
   *
   */
  def isResearch: Boolean = isInstanceOf[ResearchPlugin]

  /** A description of this plugin's options, suitable as a response
   *  to the -help command-line option.  Conventionally, the options
   *  should be listed with the `-P:plugname:` part included.
   */
  val optionsHelp: Option[String] = None
}

/** A standard plugin can be inserted into the normal compilation pipeline */
trait StandardPlugin extends Plugin {
  /** Non-research plugins should override this method to return the phases
   *
   *  @param options: commandline options to the plugin, `-P:plugname:opt1,opt2` becomes List(opt1, opt2)
   *  @return a list of phases to be added to the phase plan
   */
  def init(options: List[String]): List[PluginPhase]
}

/** A research plugin may customize the compilation pipeline freely
 *
 *  @note Research plugins are only supported by nightly or snapshot build of the compiler.
 */
trait ResearchPlugin extends Plugin {
  /** Research plugins should override this method to return the new phase plan
   *
   *  @param options: commandline options to the plugin, `-P:plugname:opt1,opt2` becomes List(opt1, opt2)
   *  @param plan: the given phase plan
   *  @return the new phase plan
   */
  def init(options: List[String], plan: List[List[Phase]])(using Context): List[List[Phase]]
}

object Plugin {

  private val PluginFile = "plugin.properties"

  /** Create a class loader with the specified locations plus
   *  the loader that loaded the Scala compiler.
   */
  private def loaderFor(locations: Seq[Path]): ClassLoader = {
    val compilerLoader = classOf[Plugin].getClassLoader
    val urls = locations map (_.toURL)

    new java.net.URLClassLoader(urls.toArray, compilerLoader)
  }

  type AnyClass = Class[?]

  /** Use a class loader to load the plugin class.
   */
  def load(classname: String, loader: ClassLoader): Try[AnyClass] = {
    import scala.util.control.NonFatal
    try
      Success[AnyClass](loader loadClass classname)
    catch {
      case NonFatal(e) =>
        Failure(new PluginLoadException(classname, s"Error: unable to load class $classname: ${e.getMessage}"))
      case e: NoClassDefFoundError =>
        Failure(new PluginLoadException(classname, s"Error: class not found: ${e.getMessage} required by $classname"))
    }
  }

  /** Load all plugins specified by the arguments.
   *  Each location of `paths` must be a valid plugin archive or exploded archive.
   *  Each of `paths` must define one plugin.
   *  Each of `dirs` may be a directory containing arbitrary plugin archives.
   *  Skips all plugins named in `ignoring`.
   *  A classloader is created to load each plugin.
   */
  def loadAllFrom(
    paths: List[List[Path]],
    dirs: List[Path],
    ignoring: List[String]): List[Try[Plugin]] = {

    def fromFile(inputStream: InputStream, path: Path): String = {
      val props = new Properties
      props.load(inputStream)
      inputStream.close()

      val pluginClass = props.getProperty("pluginClass")

      if (pluginClass == null) throw new RuntimeException("Bad plugin descriptor: " + path)
      else pluginClass
    }

    def loadDescriptionFromDir(f: Path): Try[String] = {
      val path = f / PluginFile
      Try(fromFile(new java.io.FileInputStream(path.jpath.toFile), path))
    }

    def loadDescriptionFromJar(jarp: Path): Try[String] = {
      // XXX Return to this once we have more ARM support
      def read(is: InputStream) =
        if (is == null) throw new PluginLoadException(jarp.path, s"Missing $PluginFile in $jarp")
        else fromFile(is, jarp)

      val fileEntry = new java.util.jar.JarEntry(PluginFile)
      Try(read(new Jar(jarp.jpath.toFile).getEntryStream(fileEntry)))
    }

    // List[(jar, Try(descriptor))] in dir
    def scan(d: Directory) =
      d.files.toList sortBy (_.name) filter (Jar isJarOrZip _) map (j => (j, loadDescriptionFromJar(j)))

    type PDResults = List[Try[(String, ClassLoader)]]

    // scan plugin dirs for jars containing plugins, ignoring dirs with none and other jars
    val fromDirs: PDResults = dirs filter (_.isDirectory) flatMap { d =>
      scan(d.toDirectory) collect {
        case (j, Success(pd)) => Success((pd, loaderFor(Seq(j))))
      }
    }

    // scan jar paths for plugins, taking the first plugin you find.
    // a path element can be either a plugin.jar or an exploded dir.
    def findDescriptor(ps: List[Path]) = {
      def loop(qs: List[Path]): Try[String] = qs match {
        case Nil       => Failure(new MissingPluginException(ps))
        case p :: rest =>
          if (p.isDirectory) loadDescriptionFromDir(p.toDirectory) orElse loop(rest)
          else if (p.isFile) loadDescriptionFromJar(p.toFile) orElse loop(rest)
          else loop(rest)
      }
      loop(ps)
    }

    val fromPaths: PDResults = paths map (p => findDescriptor(p) match {
      case Success(classname) => Success((classname, loaderFor(p)))
      case Failure(e)  => Failure(e)
    })

    val seen = util.HashSet[String]()
    val enabled = (fromPaths ::: fromDirs) map(_.flatMap {
      case (classname, loader) =>
        Plugin.load(classname, loader).flatMap {  clazz =>
          val plugin = instantiate(clazz)
          if (seen.contains(classname))   // a nod to scala/bug#7494, take the plugin classes distinctly
            Failure(new PluginLoadException(plugin.name, s"Ignoring duplicate plugin ${plugin.name} (${classname})"))
          else if (ignoring contains plugin.name)
            Failure(new PluginLoadException(plugin.name, s"Disabling plugin ${plugin.name}"))
          else {
            seen += classname
            Success(plugin)
          }
        }
    })
    enabled   // distinct and not disabled
  }

  /** Instantiate a plugin class, given the class and
   *  the compiler it is to be used in.
   */
  def instantiate(clazz: AnyClass): Plugin = clazz.getConstructor().newInstance().asInstanceOf[Plugin]
}

class PluginLoadException(val path: String, message: String, cause: Exception) extends Exception(message, cause) {
  def this(path: String, message: String) = this(path, message, null)
}

class MissingPluginException(path: String) extends PluginLoadException(path, s"No plugin in path $path") {
  def this(paths: List[Path]) = this(paths mkString File.pathSeparator)
}
