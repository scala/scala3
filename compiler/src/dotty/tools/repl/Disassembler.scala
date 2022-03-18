package dotty.tools
package repl

import scala.annotation.internal.sharable
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

import dotc.core.StdNames.*
import DisResult.*

/** Abstract representation of a disassembler.
 *  The high-level disassembly process is as follows:
 *   1. parse the arguments to disassembly command
 *   2. map input targets to class bytes via DisassemblyClass
 *   3. select a DisassemblyTool implementation and run it to generate disassembly text
 *   4. perform any post-processing/filtering of the output text
 */
abstract class Disassembler:
  import Disassembler.*

  /** Run the disassembly tool with the supplied options, in the context of a DisassemblerRepl */
  def apply(opts: DisassemblerOptions)(using DisassemblerRepl): List[DisResult]

  /** A list of help strings for the flags supported by this disassembler.
   *  Each entry is of the form: "-flag" -> "help text"
   */
  def helps: List[(String, String)]

  /** Formatted help text for this disassembler. */
  def helpText: String = helps.map((name, help) => f"${name}%-12.12s${help}%s%n").mkString

  /** The post-processing filters to be applied to the text results of this disassembler,
   *  based on the options in effect and the disassembly target. The filtering of REPL
   *  naming artifacts is implemented here and enabled by the special `-filter` flag;
   *  subclasses may provide additional filters as appropriate.
   */
  def filters(target: String, opts: DisassemblerOptions): List[String => String] =
    if opts.filterReplNames then filterReplNames :: Nil else Nil

  /** Combined chain of filters for post-processing disassembly output. */
  final def outputFilter(target: String, opts: DisassemblerOptions): String => String =
    filters(target, opts) match
      case Nil => identity
      case fs => Function.chain(fs)

object Disassembler:
  @sharable private val ReplWrapperName = (
    Regex.quote(str.REPL_SESSION_LINE) + raw"\d+" + Regex.quote("$") + "?"
  ).r

  /** A filter to remove REPL wrapper names from the output. */
  def filterReplNames(in: String): String = ReplWrapperName.replaceAllIn(in, "")

  /** Utility method to perform line-by-line filtering based on a predicate. */
  def filteredLines(text: String, pred: String => Boolean): String =
    val bldr = StringBuilder()
    text.linesIterator.foreach(line =>
      if pred(line) then
        bldr.append(line).append('\n')
    )
    bldr.toString

  /** Extract any member name from a disassembly target
   *  e.g. Foo#bar. Foo# yields zero-length member part.
   */
  def splitHashMember(s: String): Option[String] =
    s.lastIndexOf('#') match
      case -1 => None
      case  i => Some(s.drop(i + 1))
end Disassembler

/** The result of a disassembly command. */
enum DisResult:
  case DisError(message: String | Null)
  case DisSuccess(target: String, output: String)

/** The REPL context used for disassembly. */
case class DisassemblerRepl(driver: ReplDriver, state: State):
  def classLoader: ClassLoader = driver.replClassLoader()(using state.context)
  def mostRecentEntry: Seq[String] = driver.disassemblyTargetsLastWrapper(state)

final case class DisassemblerOptions(flags: Seq[String], targets: Seq[String], filterReplNames: Boolean)

/** A generic option parser, the available options are taken from `helps` */
abstract class DisassemblerOptionParser(helps: List[(String, String)]):
  def defaultToolOptions: List[String]

  /** Parse the arguments to the disassembly tool.
   *  Option args start with "-", except that "-" itself denotes the last REPL result.
   */
  def parse(args: Seq[String])(using repl: DisassemblerRepl): DisassemblerOptions =
    val (options0, targets0) = args.partition(s => s.startsWith("-") && s.length > 1)
    val (options, filterReplNames) =
      val (opts, flag) = toolArgs(options0)
      (if opts.isEmpty then defaultToolOptions else opts, flag)

    // "-" may expand into multiple targets (e.g. if multiple type defs in a single wrapper)
    val targets = targets0.flatMap {
      case "-" => repl.mostRecentEntry
      case s   => Seq(s)
    }
    DisassemblerOptions(options, targets, filterReplNames)

  // split tool options from REPL's -filter flag, also take prefixes of flag names
  private def toolArgs(args: Seq[String]): (Seq[String], Boolean) =
    val (opts, rest) = args.flatMap(massage).partition(_ != "-filter")
    (opts, rest.nonEmpty)

  private def massage(arg: String): Seq[String] =
    require(arg.startsWith("-"))
    // arg matches opt "-foo/-f" if prefix of -foo or exactly -f
    val r = """(-[^/]*)(?:/(-.))?""".r

    def maybe(opt: String, s: String): Option[String] = opt match
      // disambiguate by preferring short form
      case r(lf, sf) if s == sf         => Some(sf)
      case r(lf, sf) if lf startsWith s => Some(lf)
      case _                            => None

    def candidates(s: String) = helps.map(h => maybe(h._1, s)).flatten

    // one candidate or one single-char candidate
    def uniqueOf(maybes: Seq[String]) =
      def single(s: String) = s.length == 2
      if maybes.length == 1 then maybes
      else if maybes.count(single) == 1 then maybes.filter(single)
      else Nil

    // each optchar must decode to exactly one option
    def unpacked(s: String): Try[Seq[String]] =
      val ones = s.drop(1).map(c =>
        val maybes = uniqueOf(candidates(s"-$c"))
        if maybes.length == 1 then Some(maybes.head) else None
      )
      Try(ones) filter (_ forall (_.isDefined)) map (_.flatten)

    val res = uniqueOf(candidates(arg))
    if res.nonEmpty then res
    else unpacked(arg).getOrElse(Seq("-help")) // or else someone needs help
  end massage
end DisassemblerOptionParser

/** A tool to perform disassembly of class bytes. */
abstract class DisassemblyTool:
  import DisassemblyTool.*
  def apply(options: Seq[String])(inputs: Seq[Input]): List[DisResult]

object DisassemblyTool:
  /** The input to a disassembly tool.
   *
   *  @param target  The disassembly target as given by the user.
   *  @param actual  The class name or file name where the target data was found.
   *  @param data    The class bytes to be disassembled.
   */
  case class Input(target: String, actual: String, data: Try[Array[Byte]])

/** A provider of the bytes to be disassembled.
 *
 *  Handles translation of an input path to a (possible empty) array of bytes
 *  from the specified classloader, where the input path may be:
 *   - a class name (possibly qualified)
 *   - the name of a type or term symbol in scope
 *   - the filesystem path to a .class file
 *
 *  The REPL uses an in-memory classloader, so depending on the target of the
 *  disassembly, the bytes under examination may not exist on disk.
 */
class DisassemblyClass(loader: ClassLoader)(using repl: DisassemblerRepl):
  import DisassemblyClass.*
  import DisassemblyTool.*
  import dotty.tools.io.File
  import dotty.tools.runner.ClassLoaderOps.*
  import java.io.FileNotFoundException

  /** Associate the requested path with a possibly failed or empty array of bytes. */
  def bytes(path: String): Input =
    bytesFor(path) match
      case Success((actual, bytes)) => Input(path, actual, Success(bytes))
      case Failure(ex)              => Input(path, path, Failure(ex))

  /** Find bytes. Handle "Foo#bar" (by ignoring member), "#bar" (by taking "bar").
   *  @return the path to use for filtering, and the byte array
   */
  private def bytesFor(path: String) =
    import scala.language.unsafeNulls // lampepfl/dotty#14672
    Try {
      path match
        case HashSplit(prefix, _) if prefix != null => prefix
        case HashSplit(_, member) if member != null => member
        case s                                      => s
    }.flatMap(findBytes)

  // data paired with actual path where it was found
  private def findBytes(path: String) = tryFile(path) orElse tryClass(path)

  /** Assume the string is a path and try to find the classfile it represents. */
  private def tryFile(path: String): Try[(String, Array[Byte])] =
    Try(File(path.asClassResource))
      .filter(_.exists)
      .map(f => (path, f.toByteArray()))

  /** Assume the string is a fully qualified class name and try to
   *  find the class object it represents.
   *  There are other symbols of interest, too:
   *  - a definition that is wrapped in an enclosing class
   *  - a synthetic that is not in scope but its associated class is
   */
  private def tryClass(path: String): Try[(String, Array[Byte])] =
    given State = repl.state

    def loadable(name: String) = loader.resourceable(name)

    // if path has an interior dollar, take it as a synthetic
    // if the prefix up to the dollar is a symbol in scope,
    // result is the translated prefix + suffix
    def desynthesize(s: String): Option[String] =
      val i = s.indexOf('$')
      if 0 until s.length - 1 contains i then
        val name = s.substring(0, i).nn
        val sufx = s.substring(i)

        def loadableOrNone(strip: Boolean) =
          def suffix(strip: Boolean)(x: String) =
            (if strip && x.endsWith("$") then x.init else x) + sufx
          repl.driver.binaryClassOfType(name)
            .map(suffix(strip)(_))
            .filter(loadable)

        // try loading translated+suffix
        // some synthetics lack a dollar, (e.g., suffix = delayedInit$body)
        // so as a hack, if prefix$$suffix fails, also try prefix$suffix
        loadableOrNone(strip = false)
          .orElse(loadableOrNone(strip = true))
      else
        None
    end desynthesize

    def scopedClass(name: String): Option[String] = repl.driver.binaryClassOfType(name).filter(loadable)
    def enclosingClass(name: String): Option[String] = repl.driver.binaryClassOfTerm(name).filter(loadable)
    def qualifiedName(name: String): Option[String] = Some(name).filter(_.contains('.')).filter(loadable)

    val p = path.asClassName   // scrub any suffix
    val className =
      qualifiedName(p)
      .orElse(scopedClass(p))
      .orElse(enclosingClass(p))
      .orElse(desynthesize(p))
      .getOrElse(p)

    val classBytes = loader.classBytes(className)
    if classBytes.isEmpty then
      Failure(FileNotFoundException(s"Could not find class bytes for '$path'"))
    else
      Success(className, classBytes)
  end tryClass

object DisassemblyClass:
  private final val classSuffix = ".class"

  /** Match foo#bar, both groups are optional (may be null). */
  @sharable private val HashSplit = "([^#]+)?(?:#(.+)?)?".r

  // We enjoy flexibility in specifying either a fully-qualified class name com.acme.Widget
  // or a resource path com/acme/Widget.class; but not widget.out
  extension (s: String)
    def asClassName = s.stripSuffix(classSuffix).replace('/', '.').nn
    def asClassResource = if s.endsWith(classSuffix) then s else s.replace('.', '/').nn + classSuffix

  extension (cl: ClassLoader)
    /** Would classBytes succeed with a nonempty array */
    def resourceable(className: String): Boolean =
      cl.getResource(className.asClassResource) != null
end DisassemblyClass
