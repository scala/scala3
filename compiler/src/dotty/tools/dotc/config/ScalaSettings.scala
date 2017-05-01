package dotty.tools.dotc
package config

import java.io.File

import PathResolver.Defaults
import rewrite.Rewrites

class ScalaSettings extends Settings.SettingGroup {

  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Path related settings */
  val bootclasspath = PathSetting("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath)
  val extdirs = PathSetting("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs)
  val javabootclasspath = PathSetting("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath)
  val javaextdirs = PathSetting("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs)
  val sourcepath = PathSetting("-sourcepath", "Specify location(s) of source files.", "") // Defaults.scalaSourcePath
  val classpath = PathSetting("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp"
  val d = StringSetting("-d", "directory|jar", "destination for generated classfiles.", ".")
  val priorityclasspath = PathSetting("-priorityclasspath", "class path that takes precedence over all other paths (or testing only)", "")

  /** Other settings */
  val deprecation = BooleanSetting("-deprecation", "Emit warning and location for usages of deprecated APIs.")
  val migration = BooleanSetting("-migration", "Emit warning and location for migration issues from Scala 2.")
  val encoding = StringSetting("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding)
  val explainTypes = BooleanSetting("-explain-types", "Explain type errors in more detail.")
  val explainImplicits = BooleanSetting("-explain-implicits", "Explain implicit search errors in more detail.")
  val explain = BooleanSetting("-explain", "Explain errors in more detail.")
  val feature = BooleanSetting("-feature", "Emit warning and location for usages of features that should be imported explicitly.")
  val help = BooleanSetting("-help", "Print a synopsis of standard options")
  val color = ChoiceSetting("-color", "mode", "Colored output", List("always", "never"/*, "auto"*/), "always"/* "auto"*/)
  val target = ChoiceSetting("-target", "target", "Target platform for object files. All JVM 1.5 targets are deprecated.",
    List("jvm-1.5", "jvm-1.5-fjbg", "jvm-1.5-asm", "jvm-1.6", "jvm-1.7", "jvm-1.8", "msil"), "jvm-1.8")
  val unchecked = BooleanSetting("-unchecked", "Enable additional warnings where generated code depends on assumptions.")
  val uniqid = BooleanSetting("-uniqid", "Uniquely tag all identifiers in debugging output.")
  val usejavacp = BooleanSetting("-usejavacp", "Utilize the java.class.path in classpath resolution.")
  val verbose = BooleanSetting("-verbose", "Output messages about what the compiler is doing.")
  val version = BooleanSetting("-version", "Print product version and exit.")
  val pageWidth = IntSetting("-pagewidth", "Set page width", 80)
  val strict = BooleanSetting("-strict", "Use strict type rules, which means some formerly legal code does not typecheck anymore.")
  val language = MultiStringSetting("-language", "feature", "Enable one or more language features.")
  val rewrite = OptionSetting[Rewrites]("-rewrite", "When used in conjunction with -language:Scala2 rewrites sources to migrate to new syntax")

  /** -X "Advanced" settings
   */
  val Xhelp = BooleanSetting("-X", "Print a synopsis of advanced options.")
  val noForwarders = BooleanSetting("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val XminImplicitSearchDepth = IntSetting("-Xmin-implicit-search-depth", "Set number of levels of implicit searches undertaken before checking for divergence.", 5)
  val xmaxInlines = IntSetting("-Xmax-inlines", "Maximal number of successive inlines", 32)
  val maxClassfileName = IntSetting("-Xmax-classfile-name", "Maximum filename length for generated classes", 255, 72 to 255)
  val Xmigration = VersionSetting("-Xmigration", "Warn about constructs whose behavior may have changed since version.")
  val Xprint = PhasesSetting("-Xprint", "Print out program after")
  val printtypes = BooleanSetting("-Xprint-types", "Print tree types (debugging option).")
  val XprintDiff = BooleanSetting("-Xprint-diff", "Print changed parts of the tree since last print.")
  val XprintDiffDel = BooleanSetting("-Xprint-diff-del", "Print chaged parts of the tree since last print including deleted parts.")
  val prompt = BooleanSetting("-Xprompt", "Display a prompt after each error (debugging option).")
  val mainClass = StringSetting("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val XnoValueClasses = BooleanSetting("-Xno-value-classes", "Do not use value classes. Helps debugging.")
  val XreplLineWidth = IntSetting("-Xrepl-line-width", "Maximial number of columns per line for REPL output", 390)
  val XfatalWarnings = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")

  /** -Y "Private" settings */
  val overrideVars = BooleanSetting("-Yoverride-vars", "Allow vars to be overridden.")
  val Yhelp = BooleanSetting("-Y", "Print a synopsis of private options.")
  val Ycheck = PhasesSetting("-Ycheck", "Check the tree at the end of")
  val YcheckMods = BooleanSetting("-Ycheck-mods", "Check that symbols and their defining trees have modifiers in sync")
  val debug = BooleanSetting("-Ydebug", "Increase the quantity of debugging output.")
  val debugAlias = BooleanSetting("-Ydebug-alias", "Never follow alias when printing types")
  val debugTrace = BooleanSetting("-Ydebug-trace", "Trace core operations")
  val debugFlags = BooleanSetting("-Ydebug-flags", "Print all flags of definitions")
  val debugNames = BooleanSetting("-Ydebug-names", "Show internal representation of names")
  val debugOwners = BooleanSetting("-Ydebug-owners", "Print all owners of definitions (requires -Yprint-syms)")
  val termConflict = ChoiceSetting("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val log = PhasesSetting("-Ylog", "Log operations during")
  val emitTasty = BooleanSetting("-YemitTasty", "Generate tasty in separate *.tasty file.")
  val Ylogcp = BooleanSetting("-Ylog-classpath", "Output information about what classpath is being applied.")
  val YdisableFlatCpCaching  = BooleanSetting("-YdisableFlatCpCaching", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.")

  val YnoImports = BooleanSetting("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val YnoPredef = BooleanSetting("-Yno-predef", "Compile without importing Predef.")
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
  val YkeepComments = BooleanSetting("-Ykeep-comments", "Keep comments when scanning source files.")
  val YforceSbtPhases = BooleanSetting("-Yforce-sbt-phases", "Run the phases used by sbt for incremental compilation (ExtractDependencies and ExtractAPI) even if the compiler is ran outside of sbt, for debugging.")
  val YdumpSbtInc = BooleanSetting("-Ydump-sbt-inc", "For every compiled foo.scala, output the API representation and dependencies used for sbt incremental compilation in foo.inc, implies -Yforce-sbt-phases.")
  val YcheckAllPatmat = BooleanSetting("-Ycheck-all-patmat", "Check exhaustivity and redundancy of all pattern matching (used for testing the algorithm)")

  /** Area-specific debug output */
  val Yexplainlowlevel = BooleanSetting("-Yexplain-lowlevel", "When explaining type errors, show types at a lower level.")
  val YnoDoubleBindings = BooleanSetting("-Yno-double-bindings", "Assert no namedtype is bound twice (should be enabled only if program is error-free).")
  val YshowVarBounds = BooleanSetting("-Yshow-var-bounds", "Print type variables with their bounds")
  val YnoInline = BooleanSetting("-Yno-inline", "Suppress inlining.")

  /** Linker specific flags */
  val optimise = BooleanSetting("-optimise", "Generates faster bytecode by applying optimisations to the program") withAbbreviation "-optimize"

  /** Dottydoc specific settings */
  val siteRoot = StringSetting(
    "-siteroot",
    "site root",
    "A directory containing static files from which to generate documentation",
    sys.props("user.dir")
  )

  val projectName = StringSetting (
    "-project",
    "project title",
    "The name of the project",
    sys.props("user.dir").split(File.separatorChar).last
  )

  val wikiSyntax = BooleanSetting("-Xwiki-syntax", "Retains the Scala2 behavior of using Wiki Syntax in Scaladoc")
}
