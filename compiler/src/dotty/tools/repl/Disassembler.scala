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

/** A disassembler implemented using the `javap` tool from the JDK
 *  TODO: note the various mechanisms/providers that may be used.
 */
object Javap extends Disassembler:
  import Disassembler.*
  import dotc.core.Decorators.*
  import dotc.core.NameOps.*

  // load and run a tool
  def apply(opts: DisassemblerOptions)(using DisassemblerRepl): List[DisResult] =
    selectProvider
      .map(_.runOn(opts))
      .getOrElse(DisError("Unable to locate a usable javap provider.") :: Nil)

  def selectProvider(using repl: DisassemblerRepl): Option[JavapProvider] =
    def maybeTask = JavapTaskProvider(repl).probed()
    def maybeTool = JavapToolProvider(repl).probed()
    maybeTask orElse maybeTool

  // Filtering output. Additional filters provided by this disassembler:
  //  - limit the disassembly output to selected method in the target, e.g. Klass#method

  override def filters(target: String, opts: DisassemblerOptions): List[String => String] =
    val commonFilters = super.filters(target, opts)
    if target.indexOf('#') != -1 then filterSelection(target) :: commonFilters
    else commonFilters

  // filter lines of javap output for target such as Klass#method
  def filterSelection(target: String)(text: String): String =
    // take Foo# as Foo#apply for purposes of filtering.
    val filterOn  = splitHashMember(target).map(s => if s.isEmpty then "apply" else s)
    var filtering = false   // true if in region matching filter

    // turn filtering on/off given the pattern of interest
    def filterStatus(line: String, pattern: String) =
      def isSpecialized(method: String) = method.startsWith(pattern + "$") && method.endsWith("$sp")
      def isAnonymized(method: String)  = (pattern == str.ANON_FUN) && method.toTermName.isAnonymousFunctionName

      // cheap heuristic, todo maybe parse for the java sig.
      // method sigs end in paren semi
      def isAnyMethod = line.endsWith(");")

      // take the method name between the space char and left paren.
      // accept exact match or something that looks like what we might be asking for.
      def isOurMethod =
        val lparen = line.lastIndexOf('(')
        val blank  = line.lastIndexOf(' ', lparen)
        if blank < 0 then false
        else
          val method = line.substring(blank + 1, lparen).nn
          (method == pattern || isSpecialized(method) || isAnonymized(method))

      // next blank line or line containing only "}" terminates section
      def isSectionEnd = line == "}" || line.trim.nn.isEmpty

      filtering =
        if filtering then
          // in non-verbose mode, next line is next method, more or less
          !isSectionEnd && (!isAnyMethod || isOurMethod)
        else
          isAnyMethod && isOurMethod

      filtering
    end filterStatus

    // do we output this line?
    def checkFilter(line: String) = filterOn.map(filterStatus(line, _)).getOrElse(true)
    filteredLines(text, checkFilter)
  end filterSelection

  // Help/options
  val helps = List(
    "usage"       -> ":javap [opts] [path or class or -]...",
    "-help"       -> "Prints this help message",
    "-verbose/-v" -> "Stack size, number of locals, method args",
    "-private/-p" -> "Private classes and members",
    "-package"    -> "Package-private classes and members",
    "-protected"  -> "Protected classes and members",
    "-public"     -> "Public classes and members",
    "-l"          -> "Line and local variable tables",
    "-c"          -> "Disassembled code",
    "-s"          -> "Internal type signatures",
    "-sysinfo"    -> "System info of class",
    "-constants"  -> "Static final constants",
    "-filter"     -> "Filter REPL machinery from output",
  )
end Javap

object JavapOptions extends DisassemblerOptionParser(Javap.helps):
  val defaultToolOptions = List("-protected", "-verbose")

/** A provider of a javap-based DisassemblyTool implementation.
 *  The provider selected is influenced by the JDK in use, and probed for availability.
 */
abstract class JavapProvider(protected val repl: DisassemblerRepl):
  /** The classloader from which the tool is loaded. */
  def loader: Either[String, ClassLoader]

  /** Find the javap tool, which may be unavailable or fail to load. */
  def findTool(loader: ClassLoader): Either[String, DisassemblyTool]

  /** Returns Some(this) if this provider is available at runtime, otherwise None.
   *  Note that this may attempt to reflectively load and initialize classes from
   *  `loader` to confirm the availability of the provider.
   */
  def probed(): Option[this.type]

  /** Run the tool on the class bytes of each target and collect results. */
  final def runOn(opts: DisassemblerOptions): List[DisResult] =
    val results = for
      cl <- loader
      tool <- findTool(cl)
      clazz = DisassemblyClass(cl)(using repl)
    yield tool(opts.flags)(opts.targets.map(clazz.bytes(_)))
    results.fold(msg => List(DisError(msg)), identity)
end JavapProvider

/** JDK9+ to locate ToolProvider. */
class JavapToolProvider(repl0: DisassemblerRepl) extends JavapProvider(repl0):
  import java.io.{ByteArrayOutputStream, PrintStream}
  import java.nio.file.Files
  import java.util.Optional
  import scala.reflect.Selectable.reflectiveSelectable
  import scala.util.Properties.isJavaAtLeast
  import scala.util.Using
  import dotty.tools.io.File
  import DisassemblyTool.Input

  type ToolProvider = { def run(out: PrintStream, err: PrintStream, args: Array[String]): Unit }

  override def loader: Either[String, ClassLoader] = Right(repl.classLoader)

  override def probed(): Option[this.type] = Option.when(isJavaAtLeast("9"))(this)

  private def createTool(provider: ToolProvider) = new DisassemblyTool:
    override def apply(options: Seq[String])(inputs: Seq[Input]): List[DisResult] =
      def runInput(input: Input): DisResult = input match
        case Input(target, _, Success(bytes)) => runFromTempFile(target, bytes)
        case Input(_, _, Failure(e))          => DisError(e.toString)

      def runFromTempFile(target: String, bytes: Array[Byte]): DisResult =
        Try(File(Files.createTempFile("scala3-repl-javap", ".class").nn)) match
          case Success(tmp) =>
            val res = Using(tmp.bufferedOutput())(_.write(bytes)) match
              case Success(_) => runTool(target, options :+ tmp.path)
              case Failure(e) => DisError(e.toString)
            tmp.delete()
            res
          case Failure(e) => DisError(e.toString)

      def runTool(target: String, args: Seq[String]): DisResult =
        val out = ByteArrayOutputStream()
        val outPrinter = PrintStream(out)
        val rc =
          reflectiveSelectable(provider)   // work around lampepfl/dotty#11043
            .applyDynamic("run", classOf[PrintStream], classOf[PrintStream], classOf[Array[String]])
            (outPrinter, outPrinter, args.toArray)
        val output = out.toString
        if rc == 0 then DisSuccess(target, output) else DisError(output)

      inputs.map(runInput).toList
    end apply
  end createTool

  //ToolProvider.findFirst("javap")
  override def findTool(loader: ClassLoader): Either[String, DisassemblyTool] =
    val provider = Class.forName("java.util.spi.ToolProvider", /*initialize=*/ true, loader).nn
      .getDeclaredMethod("findFirst", classOf[String]).nn
      .invoke(null, "javap")
      .asInstanceOf[Optional[ToolProvider]]
    if provider.isPresent then Right(createTool(provider.get.nn))
    else Left(s":javap unavailable: provider not found")
end JavapToolProvider

/** A JavapProvider using the JDK internal API JavapTask as its DisassemblyTool.
 *  This may require adding the JDK tools.jar to the classpath.
 *  The JavapTask API may not be available (especially on newer JDKs),
 *  which will disqualify this provider.
 */
class JavapTaskProvider(repl0: DisassemblerRepl) extends JavapProvider(repl0):
  import scala.util.Properties.{isJavaAtLeast, jdkHome}
  import dotty.tools.dotc.config.PathResolver
  import dotty.tools.io.File
  import dotty.tools.runner.ClassLoaderOps.*
  import java.net.URL

  private val isJava8 = !isJavaAtLeast("9")

  private def findToolsJar(): Option[File] =
    if isJava8 then PathResolver.SupplementalLocations.platformTools
    else None

  private def addToolsJarToLoader(): ClassLoader =
    findToolsJar() match
      case Some(tools) => java.net.URLClassLoader(Array[URL | Null](tools.toURL), repl.classLoader)
      case _           => repl.classLoader

  override def loader: Either[String, ClassLoader] =
    def noTools = if isJava8 then s" or no tools.jar at $jdkHome" else ""
    Right(addToolsJarToLoader()).filterOrElse(
      _.tryToInitializeClass[AnyRef](JavapTask.taskClassName).isDefined,
      s":javap unavailable: no ${JavapTask.taskClassName}$noTools"
    )

  override def findTool(loader: ClassLoader): Either[String, DisassemblyTool] =
    Try(JavapTask(loader)) match
      case Success(tool) => Right(tool)
      case Failure(e)    => Left(e.toString)

  override def probed(): Option[this.type] =
    if isJava8 then Some(this)
    else Option.when(loader.flatMap(cl => findTool(cl)).isRight)(this)
end JavapTaskProvider

/** A DisassemblyTool implementation using the JDK internal API `JavapTask`
 *  introduced by JDK7. This API supports consuming an array of bytes as
 *  its input. The task is run using reflection.
 *
 *  This approach may be unusable in JDK+ (and especially JDK 16+) due to the
 *  Java module system and ever-increasing restrictions.
 */
class JavapTask(val loader: ClassLoader) extends DisassemblyTool:
  import javax.tools.{Diagnostic, DiagnosticListener,
                      ForwardingJavaFileManager, JavaFileManager, JavaFileObject,
                      SimpleJavaFileObject, StandardLocation}
  import java.io.{CharArrayWriter, PrintWriter}
  import java.util.Locale
  import java.util.concurrent.ConcurrentLinkedQueue
  import scala.jdk.CollectionConverters.*
  import scala.collection.mutable.Clearable
  import scala.reflect.Selectable.reflectiveSelectable
  import dotty.tools.runner.ClassLoaderOps.*
  import DisassemblyTool.*

  // output filtering support
  val writer = new CharArrayWriter
  def written() =
    writer.flush()
    val w = writer.toString
    writer.reset()
    w

  type Task = { def call(): Boolean }

  class JavaReporter extends DiagnosticListener[JavaFileObject] with Clearable:
    type D = Diagnostic[? <: JavaFileObject]
    val diagnostics = new ConcurrentLinkedQueue[D]
    override def report(d: Diagnostic[? <: JavaFileObject]) = diagnostics.add(d)
    override def clear() = diagnostics.clear()
    /** All diagnostic messages.
     *  @param locale Locale for diagnostic messages, null by default.
     */
    def messages(using locale: Locale | Null = null) =
      diagnostics.asScala.map(_.getMessage(locale)).toList

    def reportable(): String =
      import scala.util.Properties.lineSeparator
      clear()
      if messages.nonEmpty then messages.mkString("", lineSeparator, lineSeparator)
      else ""
  end JavaReporter

  val reporter = new JavaReporter

  // javax.tools.DisassemblerTool.getStandardFileManager(reporter, locale, charset)
  val defaultFileManager: JavaFileManager =
    (loader
      .tryToLoadClass[JavaFileManager]("com.sun.tools.javap.JavapFileManager").get
      .getMethod("create", classOf[DiagnosticListener[?]], classOf[PrintWriter]).nn
      .invoke(null, reporter, PrintWriter(System.err, true))
    ).asInstanceOf[JavaFileManager]

  // manages named arrays of bytes, which might have failed to load
  class JavapFileManager(val managed: Seq[Input])(delegate: JavaFileManager = defaultFileManager)
  extends ForwardingJavaFileManager[JavaFileManager](delegate):
    import JavaFileObject.Kind
    import Kind.*
    import StandardLocation.*
    import JavaFileManager.Location
    import java.net.{URI, URISyntaxException}
    import java.io.{ByteArrayInputStream, InputStream}

    // name#fragment is OK, but otherwise fragile
    def uri(name: String): URI =
      try URI(name) // URI("jfo:" + name)
      catch case _: URISyntaxException => URI("dummy")

    // look up by actual class name or by target descriptor (unused?)
    def inputNamed(name: String): Try[Array[Byte]] =
      managed.find(m => m.actual == name || m.target == name).get.data

    def managedFile(name: String, kind: Kind) = kind match
      case CLASS => fileObjectForInput(name, inputNamed(name), kind)
      case _     => null

    // todo: just wrap it as scala abstractfile and adapt it uniformly
    def fileObjectForInput(name: String, bytes: Try[Array[Byte]], kind: Kind): JavaFileObject =
      new SimpleJavaFileObject(uri(name), kind):
        override def openInputStream(): InputStream = ByteArrayInputStream(bytes.get)
        // if non-null, ClassWriter wrongly requires scheme non-null
        override def toUri: URI | Null = null
        override def getName: String = name
        // suppress
        override def getLastModified: Long = -1L
    end fileObjectForInput

    override def getJavaFileForInput(location: Location, className: String, kind: Kind): JavaFileObject | Null =
      location match
        case CLASS_PATH => managedFile(className, kind)
        case _          => null

    override def hasLocation(location: Location): Boolean =
      location match
        case CLASS_PATH => true
        case _          => false
  end JavapFileManager

  def fileManager(inputs: Seq[Input]) = JavapFileManager(inputs)()

  // ServiceLoader.load(classOf[javax.tools.DisassemblerTool])
  //   .getTask(writer, fileManager, reporter, options.asJava, classes.asJava)
  def task(options: Seq[String], classes: Seq[String], inputs: Seq[Input]): Task =
    loader.createInstance[Task]
      (JavapTask.taskClassName, Console.println(_))
      (writer, fileManager(inputs), reporter, options.asJava, classes.asJava)

  /** Run the tool. */
  override def apply(options: Seq[String])(inputs: Seq[Input]): List[DisResult] =
    def runInput(input: Input): DisResult = input match
      case Input(target, actual, Success(_)) =>
        import java.lang.reflect.InvocationTargetException
        try
          if task(options, Seq(actual), inputs).call() then
            DisSuccess(target, reporter.reportable() + written())
          else
            DisError(reporter.reportable())
        catch case e: InvocationTargetException =>
          e.getCause match
            case t: IllegalArgumentException => DisError(t.getMessage) // bad option
            case t: Throwable                => throw t
            case null                        => throw e
        finally
          reporter.clear()

      case Input(_, _, Failure(e)) =>
        DisError(e.getMessage)
    end runInput

    inputs.map(runInput).toList
  end apply

object JavapTask:
  // introduced in JDK7 as internal API
  val taskClassName = "com.sun.tools.javap.JavapTask"
end JavapTask
