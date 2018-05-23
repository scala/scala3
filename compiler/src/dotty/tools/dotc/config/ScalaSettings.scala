package dotty.tools.dotc
package config

import java.io.File
import dotty.tools.io.{ Directory, PlainDirectory }

import PathResolver.Defaults
import rewrite.Rewrites

class ScalaSettings extends Settings.SettingGroup {

  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Path related settings */
  val bootclasspath = PathSetting("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath)
  val extdirs = PathSetting("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs)
  val javabootclasspath = PathSetting("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath)
  val javaextdirs = PathSetting("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs)
  val sourcepath = PathSetting("-sourcepath", "Specify location(s) of source files.", Defaults.scalaSourcePath)
  val scansource = BooleanSetting("-scansource", "Scan source files to locate classes for which class-name != file-name")

  val classpath = PathSetting("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp"
  val outputDir = OutputSetting("-d", "directory|jar", "destination for generated classfiles.",
    new PlainDirectory(Directory(".")))
  val priorityclasspath = PathSetting("-priorityclasspath", "class path that takes precedence over all other paths (or testing only)", "")

  /** Other settings */
  val deprecation = BooleanSetting("-deprecation", "Emit warning and location for usages of deprecated APIs.")
  val migration = BooleanSetting("-migration", "Emit warning and location for migration issues from Scala 2.")
  val encoding = StringSetting("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding)
  val explainTypes = BooleanSetting("-explain-types", "Explain type errors in more detail.")
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
  val silentWarnings = BooleanSetting("-nowarn", "Silence all warnings.")
  val fromTasty = BooleanSetting("-from-tasty", "Compile classes from tasty in classpath. The arguments are used as class names.")

  /** Decompiler settings */
  val printTasty = BooleanSetting("-print-tasty", "Prints the raw tasty.")
  val printLines = BooleanSetting("-print-lines", "Show source code line numbers.")

  /** Plugin-related setting */
  val plugin             = MultiStringSetting  ("-Xplugin", "paths", "Load a plugin from each classpath.")
  val disable            = MultiStringSetting  ("-Xplugin-disable", "plugin", "Disable plugins by name.")
  val require            = MultiStringSetting  ("-Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val showPlugins        = BooleanSetting      ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val pluginsDir         = StringSetting       ("-Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val pluginOptions      = MultiStringSetting  ("-P", "plugin:opt", "Pass an option to a plugin, e.g. -P:<plugin>:<opt>")

  /** -X "Advanced" settings
   */
  val Xhelp = BooleanSetting("-X", "Print a synopsis of advanced options.")
  val XnoForwarders = BooleanSetting("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val XminImplicitSearchDepth = IntSetting("-Xmin-implicit-search-depth", "Set number of levels of implicit searches undertaken before checking for divergence.", 5)
  val XmaxInlines = IntSetting("-Xmax-inlines", "Maximal number of successive inlines", 32)
  val XmaxClassfileName = IntSetting("-Xmax-classfile-name", "Maximum filename length for generated classes", 255, 72 to 255)
  val Xmigration = VersionSetting("-Xmigration", "Warn about constructs whose behavior may have changed since version.")
  val Xprint = PhasesSetting("-Xprint", "Print out program after")
  val XprintTypes = BooleanSetting("-Xprint-types", "Print tree types (debugging option).")
  val XprintDiff = BooleanSetting("-Xprint-diff", "Print changed parts of the tree since last print.")
  val XprintDiffDel = BooleanSetting("-Xprint-diff-del", "Print changed parts of the tree since last print including deleted parts.")
  val Xprompt = BooleanSetting("-Xprompt", "Display a prompt after each error (debugging option).")
  val XmainClass = StringSetting("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val XnoValueClasses = BooleanSetting("-Xno-value-classes", "Do not use value classes. Helps debugging.")
  val XreplLineWidth = IntSetting("-Xrepl-line-width", "Maximal number of columns per line for REPL output", 390)
  val XfatalWarnings = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")
  val XverifySignatures = BooleanSetting("-Xverify-signatures", "Verify generic signatures in generated bytecode.")

  /** -Y "Private" settings */
  val YoverrideVars = BooleanSetting("-Yoverride-vars", "Allow vars to be overridden.")
  val Yhelp = BooleanSetting("-Y", "Print a synopsis of private options.")
  val Ycheck = PhasesSetting("-Ycheck", "Check the tree at the end of")
  val YcheckMods = BooleanSetting("-Ycheck-mods", "Check that symbols and their defining trees have modifiers in sync")
  val Ydebug = BooleanSetting("-Ydebug", "Increase the quantity of debugging output.")
  val YdebugTrace = BooleanSetting("-Ydebug-trace", "Trace core operations")
  val YdebugFlags = BooleanSetting("-Ydebug-flags", "Print all flags of definitions")
  val YdebugMissingRefs = BooleanSetting("-Ydebug-missing-refs", "Print a stacktrace when a required symbol is missing")
  val YdebugNames = BooleanSetting("-Ydebug-names", "Show internal representation of names")
  val YtermConflict = ChoiceSetting("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val Ylog = PhasesSetting("-Ylog", "Log operations during")
  val YemitTastyInClass = BooleanSetting("-Yemit-tasty-in-class", "Generate tasty in the .class file and add an empty *.hasTasty file.")
  val YlogClasspath = BooleanSetting("-Ylog-classpath", "Output information about what classpath is being applied.")
  val YdisableFlatCpCaching  = BooleanSetting("-YdisableFlatCpCaching", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.")

  val YnoImports = BooleanSetting("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val YnoInline = BooleanSetting("-Yno-inline", "Suppress inlining.")
  val YnoGenericSig = BooleanSetting("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val YnoPredef = BooleanSetting("-Yno-predef", "Compile without importing Predef.")
  val Yskip = PhasesSetting("-Yskip", "Skip")
  val Ydumpclasses = StringSetting("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val YstopAfter = PhasesSetting("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val YstopBefore = PhasesSetting("-Ystop-before", "Stop before") // stop before erasure as long as we have not debugged it fully
  val YtraceContextCreation = BooleanSetting("-Ytrace-context-creation", "Store stack trace of context creations.")
  val YshowSuppressedErrors = BooleanSetting("-Yshow-suppressed-errors", "Also show follow-on errors and warnings that are normally suppressed.")
  val YdetailedStats = BooleanSetting("-Ydetailed-stats", "show detailed internal compiler stats (needs Stats.enabled to be set to true).")
  val Yheartbeat = BooleanSetting("-Ydetailed-stats", "show heartbeat stack trace of compiler operations (needs Stats.enabled to be set to true).")
  val YprintPos = BooleanSetting("-Yprint-pos", "show tree positions.")
  val YprintPosSyms = BooleanSetting("-Yprint-pos-syms", "show symbol definitions positions.")
  val YnoDeepSubtypes = BooleanSetting("-Yno-deep-subtypes", "throw an exception on deep subtyping call stacks.")
  val YnoPatmatOpt = BooleanSetting("-Yno-patmat-opt", "disable all pattern matching optimizations.")
  val YplainPrinter = BooleanSetting("-Yplain-printer", "Pretty-print using a plain printer.")
  val YprintSyms = BooleanSetting("-Yprint-syms", "when printing trees print info in symbols instead of corresponding info in trees.")
  val YprintDebug = BooleanSetting("-Yprint-debug", "when printing trees, print some extra information useful for debugging.")
  val YprintDebugOwners = BooleanSetting("-Yprint-debug-owners", "when printing trees, print owners of definitions.")
  val YshowPrintErrors = BooleanSetting("-Yshow-print-errors", "don't suppress exceptions thrown during tree printing.")
  val YtestPickler = BooleanSetting("-Ytest-pickler", "self-test for pickling functionality; should be used with -Ystop-after:pickler")
  val YcheckReentrant = BooleanSetting("-Ycheck-reentrant", "check that compiled program does not contain vars that can be accessed from a global root.")
  val YdropComments = BooleanSetting("-Ydrop-comments", "Drop comments when scanning source files.")
  val YcookComments = BooleanSetting("-Ycook-comments", "Cook the comments (type check `@usecase`, etc.)")
  val YforceSbtPhases = BooleanSetting("-Yforce-sbt-phases", "Run the phases used by sbt for incremental compilation (ExtractDependencies and ExtractAPI) even if the compiler is ran outside of sbt, for debugging.")
  val YdumpSbtInc = BooleanSetting("-Ydump-sbt-inc", "For every compiled foo.scala, output the API representation and dependencies used for sbt incremental compilation in foo.inc, implies -Yforce-sbt-phases.")
  val YcheckAllPatmat = BooleanSetting("-Ycheck-all-patmat", "Check exhaustivity and redundancy of all pattern matching (used for testing the algorithm)")
  val YretainTrees = BooleanSetting("-Yretain-trees", "Retain trees for top-level classes, accessible from ClassSymbol#tree")
  val YshowTreeIds = BooleanSetting("-Yshow-tree-ids", "Uniquely tag all tree nodes in debugging output.")

  val YprofileEnabled = BooleanSetting("-Yprofile-enabled", "Enable profiling.")
  val YprofileDestination = StringSetting("-Yprofile-destination", "file", "where to send profiling output - specify a file, default is to the console.", "")
      //.withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileExternalTool = PhasesSetting("-Yprofile-external-tool", "Enable profiling for a phase using an external tool hook. Generally only useful for a single phase", "typer")
      //.withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileRunGcBetweenPhases = PhasesSetting("-Yprofile-run-gc", "Run a GC between phases - this allows heap size to be accurate at the expense of more time. Specify a list of phases, or *", "_")
      //.withPostSetHook( _ => YprofileEnabled.value = true )

  // Extremely experimental language features
  val YkindPolymorphism = BooleanSetting("-Ykind-polymorphism", "Enable kind polymorphism (see http://dotty.epfl.ch/docs/reference/kind-polymorphism.html). Potentially unsound.")

  /** Area-specific debug output */
  val YexplainLowlevel = BooleanSetting("-Yexplain-lowlevel", "When explaining type errors, show types at a lower level.")
  val YnoDoubleBindings = BooleanSetting("-Yno-double-bindings", "Assert no namedtype is bound twice (should be enabled only if program is error-free).")
  val YshowVarBounds = BooleanSetting("-Yshow-var-bounds", "Print type variables with their bounds")
  val YshowNoInline = BooleanSetting("-Yshow-no-inline", "Show inlined code without the 'inlined from' info")

  /** Linker specific flags */
  val optimise = BooleanSetting("-optimise", "Generates faster bytecode by applying local optimisations to the .program") withAbbreviation "-optimize"
  val Xlink = BooleanSetting("-Xlink", "Recompile library code with the application.")
  val YoptPhases = PhasesSetting("-Yopt-phases", "Restrict the optimisation phases to execute under -optimise.")
  val YoptFuel = IntSetting("-Yopt-fuel", "Maximum number of optimisations performed under -optimise.", -1)
  val YnoDecodeStacktraces = BooleanSetting("-Yno-decode-stacktraces", "Show raw StackOverflow stacktraces, instead of decoding them into triggering operations.")

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
    ""
  )

  val projectVersion = StringSetting (
    "-project-version",
    "project version",
    "The current version of your project",
    ""
  )

  val projectUrl = StringSetting (
    "-project-url",
    "project repository homepage",
    "The source repository of your project",
    ""
  )

  val wikiSyntax = BooleanSetting("-Xwiki-syntax", "Retains the Scala2 behavior of using Wiki Syntax in Scaladoc")
}
