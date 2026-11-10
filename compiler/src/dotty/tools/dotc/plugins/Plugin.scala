package dotty.tools.dotc
package plugins

import core.*
import Contexts.*
import Phases.*
import dotty.tools.dotc.classpath.ClassPath
import dotty.tools.nio.*
import transform.MegaPhase.MiniPhase

import java.io.InputStream
import java.util.Properties
import scala.util.{Failure, Success, Try}
import scala.annotation.nowarn
import scala.collection.mutable

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
   *  The phases returned must be freshly constructed (not reused
   *  and returned again on subsequent calls).
   *
   *  @param options commandline options to the plugin.
   *  @return a list of phases to be added to the phase plan
   */
  @deprecatedOverriding("Method 'init' does not allow to access 'Context', use 'initialize' instead.", since = "Scala 3.5.0")
  @deprecated("Use 'initialize' instead.", since = "Scala 3.5.0")
  def init(options: List[String]): List[PluginPhase] = Nil

  /** Non-research plugins should override this method to return the phases
   *
   *  The phases returned must be freshly constructed (not reused
   *  and returned again on subsequent calls).
   *
   *  @param options commandline options to the plugin.
   *  @return a list of phases to be added to the phase plan
   */
  @nowarn("cat=deprecation")
  def initialize(options: List[String])(using Context): List[PluginPhase] = init(options)
}

/** A research plugin may customize the compilation pipeline freely
 *
 *  @note Research plugins are only supported by nightly or snapshot build of the compiler.
 */
trait ResearchPlugin extends Plugin {
  /** Research plugins should override this method to return the new phase plan
   *
   *  Any plugin phases included in the plan must be freshly constructed (not reused
   *  and returned again on subsequent calls).
   *
   *  @param options commandline options to the plugin, `-P:plugname:opt1,opt2` becomes List(opt1, opt2)
   *  @param plan the given phase plan
   *  @return the new phase plan
   */
  def init(options: List[String], plan: List[List[Phase]])(using Context): List[List[Phase]]
}

object Plugin {

  private val PluginFile = "plugin.properties"

  /** Create a class loader with the specified locations plus
   *  the loader that loaded the Scala compiler.
   */
  private def loaderFor(locations: Seq[String]): ClassLoader = {
    val compilerLoader = classOf[Plugin].getClassLoader

    // we assume plugins don't use multi-release JARs
    new AbstractFileClassLoader(locations, "", compilerLoader)
  }

  type AnyClass = Class[?]

  /** Use a class loader to load the plugin class.
   */
  @nowarn("msg=Catching NoClassDefFoundError can lead to unexpected behavior") // backwards compat
  def load(classname: String, loader: ClassLoader): Try[AnyClass] = {
    try
      Success[AnyClass](loader.loadClass(classname))
    catch
      case e: Exception =>
        Failure(new PluginLoadException(classname, s"Error: unable to load class $classname: ${e.getMessage}"))
      case e: NoClassDefFoundError =>
        Failure(new PluginLoadException(classname, s"Error: class not found: ${e.getMessage} required by $classname"))
  }

  /** Load all plugins specified by the arguments.
   *  Each location of `paths` must be a valid plugin archive or exploded archive.
   *  Each of `paths` must define one plugin.
   *  Each of `dirs` may be a directory containing arbitrary plugin archives.
   *  Skips all plugins named in `ignoring`.
   *  A classloader is created to load each plugin.
   */
  def loadAllFrom(
    paths: List[List[String]],
    dirs: List[String],
    ignoring: List[String]): List[Try[Plugin]] = {

    def fromFile(inputStream: InputStream, path: String): String = {
      val props = new Properties
      props.load(inputStream)
      inputStream.close()

      val pluginClass = props.getProperty("pluginClass")

      if (pluginClass == null) throw new RuntimeException("Bad plugin descriptor: " + path)
      else pluginClass
    }

    def loadDescription(f: FileContainer): Try[String] =
      f.getFile(PluginFile) match
        case Some(f) => Try(fromFile(f.input(), f.path))
        case None => Failure(new RuntimeException("Missing plugin descriptor: " + f.path))

    // List[(jar, Try(descriptor))] in dir
    def scan(d: FileContainer) =
      d.entries.toList
        .collect { case f: File => f }
        .sortBy(_.name)
        .flatMap(FileContainer.getFromFile(_, FileContainer.DefaultJarVersion, FileContainer.DefaultCompressionLevel))
        .map(j => (j, loadDescription(j)))

    type PDResults = List[Try[(String, ClassLoader)]]

    // scan plugin dirs for jars containing plugins, ignoring dirs with none and other jars
    val fromDirs: PDResults = dirs.flatMap(FileContainer.getOnDisk).flatMap { d =>
      scan(d) collect {
        case (j, Success(pd)) => Success((pd, loaderFor(Seq(j.path))))
      }
    }

    // scan jar paths for plugins, taking the first plugin you find.
    // a path element can be either a plugin.jar or an exploded dir.
    def findDescriptor(ps: List[String]) = {
      def loop(qs: List[String]): Try[String] = qs match {
        case Nil       => Failure(new MissingPluginException(ps))
        case p :: rest =>
          FileContainer.getOnDiskIncludingFromFile(p, FileContainer.DefaultJarVersion, FileContainer.DefaultCompressionLevel) match
            case Some(c) => loadDescription(c) `orElse` loop(rest)
            case None => loop(rest)
      }
      loop(ps)
    }

    val fromPaths: PDResults = paths map (p => findDescriptor(p) match {
      case Success(classname) => Success((classname, loaderFor(p)))
      case Failure(e)  => Failure(e)
    })

    val seen = mutable.Set.empty[String]
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

class PluginLoadException(val path: String, message: String, cause: Exception | Null) extends Exception(message, cause) {
  def this(path: String, message: String) = this(path, message, null)
}

class MissingPluginException(path: String) extends PluginLoadException(path, s"No plugin in path $path") {
  def this(paths: List[String]) = this(paths.mkString(ClassPath.pathSeparator))
}
