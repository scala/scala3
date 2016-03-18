package dotty.tools.dotc
package config

import PathResolver.Defaults
import rewrite.Rewrites

class ScalaSettings extends Settings.SettingGroup {

  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Path related settings.
   */
  val bootclasspath = PathSetting("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath)
  val extdirs = PathSetting("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs)
  val javabootclasspath = PathSetting("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath)
  val javaextdirs = PathSetting("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs)
  val sourcepath = PathSetting("-sourcepath", "Specify location(s) of source files.", "") // Defaults.scalaSourcePath

  /** Other settings.
   */
  val deprecation = BooleanSetting("-deprecation", "Emit warning and location for usages of deprecated APIs.")
  val migration = BooleanSetting("-migration", "Emit warning and location for migration issues from Scala 2.")
  val explaintypes = BooleanSetting("-explaintypes", "Explain type errors in more detail.")
  val feature = BooleanSetting("-feature", "Emit warning and location for usages of features that should be imported explicitly.")
  val help = BooleanSetting("-help", "Print a synopsis of standard options")
  val target = ChoiceSetting("-target", "target", "Target platform for object files. All JVM 1.5 targets are deprecated.",
    List("jvm-1.5", "jvm-1.5-fjbg", "jvm-1.5-asm", "jvm-1.6", "jvm-1.7", "jvm-1.8", "msil"),
    "jvm-1.8")
  val scalajs = BooleanSetting("-scalajs", "Compile in Scala.js mode (requires scalajs-library.jar on the classpath).")
  val unchecked = BooleanSetting("-unchecked", "Enable additional warnings where generated code depends on assumptions.")
  val uniqid = BooleanSetting("-uniqid", "Uniquely tag all identifiers in debugging output.")
  val usejavacp = BooleanSetting("-usejavacp", "Utilize the java.class.path in classpath resolution.")
  val verbose = BooleanSetting("-verbose", "Output messages about what the compiler is doing.")
  val version = BooleanSetting("-version", "Print product version and exit.")
  val pageWidth = IntSetting("-pagewidth", "Set page width", 80)

  val jvmargs = PrefixSetting("-J<flag>", "-J", "Pass <flag> directly to the runtime system.")
  val defines = PrefixSetting("-Dproperty=value", "-D", "Pass -Dproperty=value directly to the runtime system.")
  val toolcp = PathSetting("-toolcp", "Add to the runner classpath.", "")
  val nobootcp = BooleanSetting("-nobootcp", "Do not use the boot classpath for the scala jars.")
  val strict = BooleanSetting("-strict", "Use strict type rules, which means some formerly legal code does not typecheck anymore.")

  val classpath = PathSetting("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp"
  val d = StringSetting("-d", "directory|jar", "destination for generated classfiles.", ".")
  val language = MultiStringSetting("-language", "feature", "Enable one or more language features.")
  val rewrite = OptionSetting[Rewrites]("-rewrite", "When used in conjunction with -language:Scala2 rewrites sources to migrate to new syntax")

  /** -X "Advanced" settings
   */
  val Xhelp = BooleanSetting("-X", "Print a synopsis of advanced options.")
  val noForwarders = BooleanSetting("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val XminImplicitSearchDepth = IntSetting("-Xmin-implicit-search-depth", "Set number of levels of implicit searches undertaken before checking for divergence.", 5)
  val maxClassfileName = IntSetting("-Xmax-classfile-name", "Maximum filename length for generated classes", 255, 72 to 255)
  val Xmigration = VersionSetting("-Xmigration", "Warn about constructs whose behavior may have changed since version.")
  val Xnojline = BooleanSetting("-Xnojline", "Do not use JLine for editing.")
  val Xprint = PhasesSetting("-Xprint", "Print out program after")
  val printtypes = BooleanSetting("-Xprint-types", "Print tree types (debugging option).")
  val prompt = BooleanSetting("-Xprompt", "Display a prompt after each error (debugging option).")
  val mainClass = StringSetting("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val XnoValueClasses = BooleanSetting("-Xno-value-classes", "Do not use value classes. Helps debugging.")
  val XreplLineWidth = IntSetting("-Xrepl-line-width", "Maximial number of columns per line for REPL output", 390)

  /** -Y "Private" settings
   */
  val overrideVars = BooleanSetting("-Yoverride-vars", "Allow vars to be overridden.")
  val Yhelp = BooleanSetting("-Y", "Print a synopsis of private options.")
  val Ycheck = PhasesSetting("-Ycheck", "Check the tree at the end of")
  val YcheckMods = BooleanSetting("-Ycheck-mods", "Check that symbols and their defining trees have modifiers in sync")
  val debug = BooleanSetting("-Ydebug", "Increase the quantity of debugging output.")
  val debugNames = BooleanSetting("-YdebugNames", "Show name-space indicators when printing names")
  val debugTrace = BooleanSetting("-Ydebug-trace", "Trace core operations")
  val debugFlags = BooleanSetting("-Ydebug-flags", "Print all flags of definitions")
  val debugOwners = BooleanSetting("-Ydebug-owners", "Print all owners of definitions (requires -Yprint-syms)")
  //val doc           = BooleanSetting    ("-Ydoc", "Generate documentation")
  val termConflict = ChoiceSetting("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val log = PhasesSetting("-Ylog", "Log operations during")
  val Ylogcp = BooleanSetting("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Yskip = PhasesSetting("-Yskip", "Skip")
  val Ydumpclasses = StringSetting("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val YstopAfter = PhasesSetting("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val YstopBefore = PhasesSetting("-Ystop-before", "Stop before") // stop before erasure as long as we have not debugged it fully
  val YmethodInfer = BooleanSetting("-Yinfer-argument-types", "Infer types for arguments of overriden methods.")
  val YtraceContextCreation = BooleanSetting("-Ytrace-context-creation", "Store stack trace of context creations.")
  val YshowSuppressedErrors = BooleanSetting("-Yshow-suppressed-errors", "Also show follow-on errors and warnings that are normally supressed.")
  val Yheartbeat = BooleanSetting("-Yheartbeat", "show heartbeat stack trace of compiler operations.")
  val Yprintpos = BooleanSetting("-Yprintpos", "show tree positions.")
  val YnoDeepSubtypes = BooleanSetting("-Yno-deep-subtypes", "throw an exception on deep subtyping call stacks.")
  val YplainPrinter = BooleanSetting("-Yplain-printer", "Pretty-print using a plain printer.")
  val YprintSyms = BooleanSetting("-Yprint-syms", "when printing trees print info in symbols instead of corresponding info in trees.")
  val YtestPickler = BooleanSetting("-Ytest-pickler", "self-test for pickling functionality; should be used with -Ystop-after:pickler")
  val YcheckReentrant = BooleanSetting("-Ycheck-reentrant", "check that compiled program does not contain vars that can be accessed from a global root.")
  def stop = YstopAfter

  /** Area-specific debug output.
   */
  val Yexplainlowlevel = BooleanSetting("-Yexplain-lowlevel", "When explaining type errors, show types at a lower level.")
  val YnoDoubleBindings = BooleanSetting("-Yno-double-bindings", "Assert no namedtype is bound twice (should be enabled only if program is error-free).")
  val YshowVarBounds = BooleanSetting("-Yshow-var-bounds", "Print type variables with their bounds")

  val optimise = BooleanSetting("-optimise", "Generates faster bytecode by applying optimisations to the program") withAbbreviation "-optimize"
}
