package dotty.tools.dotc
package config

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.io.{ Directory, PlainDirectory, AbstractFile }
import PathResolver.Defaults
import rewrites.Rewrites
import Settings.Setting

class ScalaSettings extends Settings.SettingGroup {

  protected def defaultClasspath: String = sys.env.getOrElse("CLASSPATH", ".")

  /** Path related settings */
  val bootclasspath: Setting[String] = PathSetting("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath) withAbbreviation "--boot-class-path"
  val extdirs: Setting[String] = PathSetting("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs) withAbbreviation "--extension-directories"
  val javabootclasspath: Setting[String] = PathSetting("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath) withAbbreviation "--java-boot-class-path"
  val javaextdirs: Setting[String] = PathSetting("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs) withAbbreviation "--java-extension-directories"
  val sourcepath: Setting[String] = PathSetting("-sourcepath", "Specify location(s) of source files.", Defaults.scalaSourcePath) withAbbreviation "--source-path"
  val scansource: Setting[Boolean] = BooleanSetting("-scansource", "Scan source files to locate classes for which class-name != file-name") withAbbreviation "--scan-source"

  val classpath: Setting[String] = PathSetting("-classpath", "Specify where to find user class files.", defaultClasspath) withAbbreviation "-cp" withAbbreviation "--class-path"
  val outputDir: Setting[AbstractFile] = OutputSetting("-d", "directory|jar", "destination for generated classfiles.",
    new PlainDirectory(Directory(".")))
  val priorityclasspath: Setting[String] = PathSetting("-priorityclasspath", "class path that takes precedence over all other paths (or testing only)", "") withAbbreviation "--priority-class-path"

  /** Other settings */
  val deprecation: Setting[Boolean] = BooleanSetting("-deprecation", "Emit warning and location for usages of deprecated APIs.") withAbbreviation "--deprecation"
  val migration: Setting[Boolean] = BooleanSetting("-migration", "Emit warning and location for migration issues from Scala 2.") withAbbreviation "--migration"
  val encoding: Setting[String] = StringSetting("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding) withAbbreviation "--encoding"
  val explainTypes: Setting[Boolean] = BooleanSetting("-explain-types", "Explain type errors in more detail.") withAbbreviation "--explain-types"
  val explain: Setting[Boolean] = BooleanSetting("-explain", "Explain errors in more detail.") withAbbreviation "--explain"
  val feature: Setting[Boolean] = BooleanSetting("-feature", "Emit warning and location for usages of features that should be imported explicitly.") withAbbreviation "--feature"
  val help: Setting[Boolean] = BooleanSetting("-help", "Print a synopsis of standard options") withAbbreviation "--help"
  val color: Setting[String] = ChoiceSetting("-color", "mode", "Colored output", List("always", "never"/*, "auto"*/), "always"/* "auto"*/) withAbbreviation "--color"
  val target: Setting[String] = ChoiceSetting("-target", "target", "Target platform for object files. All JVM 1.5 targets are deprecated.",
    List("jvm-1.5", "jvm-1.5-fjbg", "jvm-1.5-asm", "jvm-1.6", "jvm-1.7", "jvm-1.8", "msil"), "jvm-1.8") withAbbreviation "--target"
  val scalajs: Setting[Boolean] = BooleanSetting("-scalajs", "Compile in Scala.js mode (requires scalajs-library.jar on the classpath).") withAbbreviation "--scalajs"
  val unchecked: Setting[Boolean] = BooleanSetting("-unchecked", "Enable additional warnings where generated code depends on assumptions.") withAbbreviation "--unchecked"
  val uniqid: Setting[Boolean] = BooleanSetting("-uniqid", "Uniquely tag all identifiers in debugging output.") withAbbreviation "--unique-id"
  val usejavacp: Setting[Boolean] = BooleanSetting("-usejavacp", "Utilize the java.class.path in classpath resolution.") withAbbreviation "--use-java-class-path"
  val verbose: Setting[Boolean] = BooleanSetting("-verbose", "Output messages about what the compiler is doing.") withAbbreviation "--verbose"
  val version: Setting[Boolean] = BooleanSetting("-version", "Print product version and exit.") withAbbreviation "--version"
  val pageWidth: Setting[Int] = IntSetting("-pagewidth", "Set page width", 80) withAbbreviation "--page-width"
  val strict: Setting[Boolean] = BooleanSetting("-strict", "Use strict type rules, which means some formerly legal code does not typecheck anymore.") withAbbreviation "--strict"
  val language: Setting[List[String]] = MultiStringSetting("-language", "feature", "Enable one or more language features.") withAbbreviation "--language"
  val rewrite: Setting[Option[Rewrites]] = OptionSetting[Rewrites]("-rewrite", "When used in conjunction with -language:Scala2 rewrites sources to migrate to new syntax") withAbbreviation "--rewrite"
  val silentWarnings: Setting[Boolean] = BooleanSetting("-nowarn", "Silence all warnings.") withAbbreviation "--no-warnings"
  val fromTasty: Setting[Boolean] = BooleanSetting("-from-tasty", "Compile classes from tasty in classpath. The arguments are used as class names.") withAbbreviation "--from-tasty"

  /** Decompiler settings */
  val printTasty: Setting[Boolean] = BooleanSetting("-print-tasty", "Prints the raw tasty.") withAbbreviation "--print-tasty"
  val printLines: Setting[Boolean] = BooleanSetting("-print-lines", "Show source code line numbers.") withAbbreviation "--print-lines"

  /** Plugin-related setting */
  val plugin: Setting[List[String]]             = MultiStringSetting  ("-Xplugin", "paths", "Load a plugin from each classpath.")
  val disable: Setting[List[String]]            = MultiStringSetting  ("-Xplugin-disable", "plugin", "Disable plugins by name.")
  val require: Setting[List[String]]            = MultiStringSetting  ("-Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val showPlugins: Setting[Boolean]        = BooleanSetting      ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val pluginsDir: Setting[String]         = StringSetting       ("-Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val pluginOptions: Setting[List[String]]      = MultiStringSetting  ("-P", "plugin:opt", "Pass an option to a plugin, e.g. -P:<plugin>:<opt>")

  /** -X "Advanced" settings
   */
  val Xhelp: Setting[Boolean] = BooleanSetting("-X", "Print a synopsis of advanced options.")
  val XnoForwarders: Setting[Boolean] = BooleanSetting("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val XmaxInlines: Setting[Int] = IntSetting("-Xmax-inlines", "Maximal number of successive inlines", 32)
  val Xmigration: Setting[ScalaVersion] = VersionSetting("-Xmigration", "Warn about constructs whose behavior may have changed since version.")
  val Xprint: Setting[List[String]] = PhasesSetting("-Xprint", "Print out program after")
  val XprintTypes: Setting[Boolean] = BooleanSetting("-Xprint-types", "Print tree types (debugging option).")
  val XprintDiff: Setting[Boolean] = BooleanSetting("-Xprint-diff", "Print changed parts of the tree since last print.")
  val XprintDiffDel: Setting[Boolean] = BooleanSetting("-Xprint-diff-del", "Print changed parts of the tree since last print including deleted parts.")
  val XprintInline: Setting[Boolean] = BooleanSetting("-Xprint-inline", "Show  where inlined code comes from")
  val Xprompt: Setting[Boolean] = BooleanSetting("-Xprompt", "Display a prompt after each error (debugging option).")
  val XmainClass: Setting[String] = StringSetting("-Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val XnoValueClasses: Setting[Boolean] = BooleanSetting("-Xno-value-classes", "Do not use value classes. Helps debugging.")
  val XreplLineWidth: Setting[Int] = IntSetting("-Xrepl-line-width", "Maximal number of columns per line for REPL output", 390)
  val XfatalWarnings: Setting[Boolean] = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")
  val XverifySignatures: Setting[Boolean] = BooleanSetting("-Xverify-signatures", "Verify generic signatures in generated bytecode.")
  val XignoreScala2Macros: Setting[Boolean] = BooleanSetting("-Xignore-scala2-macros", "Ignore errors when compiling code that calls Scala2 macros, these will fail at runtime.")

  val XmixinForceForwarders = ChoiceSetting(
    name    = "-Xmixin-force-forwarders",
    helpArg = "mode",
    descr   = "Generate forwarder methods in classes inhering concrete methods from traits.",
    choices = List("true", "junit", "false"),
    default = "true")

  object mixinForwarderChoices {
    def isTruthy(implicit ctx: Context) = XmixinForceForwarders.value == "true"
    def isAtLeastJunit(implicit ctx: Context) = isTruthy || XmixinForceForwarders.value == "junit"
  }

  /** -Y "Private" settings */
  val YoverrideVars: Setting[Boolean] = BooleanSetting("-Yoverride-vars", "Allow vars to be overridden.")
  val Yhelp: Setting[Boolean] = BooleanSetting("-Y", "Print a synopsis of private options.")
  val Ycheck: Setting[List[String]] = PhasesSetting("-Ycheck", "Check the tree at the end of")
  val YcheckMods: Setting[Boolean] = BooleanSetting("-Ycheck-mods", "Check that symbols and their defining trees have modifiers in sync")
  val Ydebug: Setting[Boolean] = BooleanSetting("-Ydebug", "Increase the quantity of debugging output.")
  val YdebugTrace: Setting[Boolean] = BooleanSetting("-Ydebug-trace", "Trace core operations")
  val YdebugFlags: Setting[Boolean] = BooleanSetting("-Ydebug-flags", "Print all flags of definitions")
  val YdebugMissingRefs: Setting[Boolean] = BooleanSetting("-Ydebug-missing-refs", "Print a stacktrace when a required symbol is missing")
  val YdebugNames: Setting[Boolean] = BooleanSetting("-Ydebug-names", "Show internal representation of names")
  val YdebugPos: Setting[Boolean] = BooleanSetting("-Ydebug-pos", "Show full source positions including spans")
  val YdebugTreeWithId: Setting[Int] = IntSetting("-Ydebug-tree-with-id", "Print the stack trace when the tree with the given id is created", Int.MinValue)
  val YtermConflict: Setting[String] = ChoiceSetting("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val Ylog: Setting[List[String]] = PhasesSetting("-Ylog", "Log operations during")
  val YemitTastyInClass: Setting[Boolean] = BooleanSetting("-Yemit-tasty-in-class", "Generate tasty in the .class file and add an empty *.hasTasty file.")
  val YlogClasspath: Setting[Boolean] = BooleanSetting("-Ylog-classpath", "Output information about what classpath is being applied.")
  val YdisableFlatCpCaching: Setting[Boolean] = BooleanSetting("-YdisableFlatCpCaching", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.")

  val Yscala2Unpickler: Setting[String] = StringSetting("-Yscala2-unpickler", "", "Control where we may get Scala 2 symbols from. This is either \"always\", \"never\", or a classpath.", "always")
  // TODO: Remove once we drop support for 2.12 standard library
  val YnewCollections: Setting[Boolean] = BooleanSetting("-Ynew-collections", "Inform the compiler that we are using the 2.13 collection library (even if the 2.12 library is on the classpath).")

  val YnoImports: Setting[Boolean] = BooleanSetting("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val YnoInline: Setting[Boolean] = BooleanSetting("-Yno-inline", "Suppress inlining.")
  val YnoGenericSig: Setting[Boolean] = BooleanSetting("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val YnoPredef: Setting[Boolean] = BooleanSetting("-Yno-predef", "Compile without importing Predef.")
  val Yskip: Setting[List[String]] = PhasesSetting("-Yskip", "Skip")
  val Ydumpclasses: Setting[String] = StringSetting("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val YstopAfter: Setting[List[String]] = PhasesSetting("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val YstopBefore: Setting[List[String]] = PhasesSetting("-Ystop-before", "Stop before") // stop before erasure as long as we have not debugged it fully
  val YtraceContextCreation: Setting[Boolean] = BooleanSetting("-Ytrace-context-creation", "Store stack trace of context creations.")
  val YshowSuppressedErrors: Setting[Boolean] = BooleanSetting("-Yshow-suppressed-errors", "Also show follow-on errors and warnings that are normally suppressed.")
  val YdetailedStats: Setting[Boolean] = BooleanSetting("-Ydetailed-stats", "show detailed internal compiler stats (needs Stats.enabled to be set to true).")
  val Yheartbeat: Setting[Boolean] = BooleanSetting("-Yheartbeat", "show heartbeat stack trace of compiler operations (needs Stats.enabled to be set to true).")
  val YprintPos: Setting[Boolean] = BooleanSetting("-Yprint-pos", "show tree positions.")
  val YprintPosSyms: Setting[Boolean] = BooleanSetting("-Yprint-pos-syms", "show symbol definitions positions.")
  val YnoDeepSubtypes: Setting[Boolean] = BooleanSetting("-Yno-deep-subtypes", "throw an exception on deep subtyping call stacks.")
  val YnoPatmatOpt: Setting[Boolean] = BooleanSetting("-Yno-patmat-opt", "disable all pattern matching optimizations.")
  val YplainPrinter: Setting[Boolean] = BooleanSetting("-Yplain-printer", "Pretty-print using a plain printer.")
  val YprintSyms: Setting[Boolean] = BooleanSetting("-Yprint-syms", "when printing trees print info in symbols instead of corresponding info in trees.")
  val YprintDebug: Setting[Boolean] = BooleanSetting("-Yprint-debug", "when printing trees, print some extra information useful for debugging.")
  val YprintDebugOwners: Setting[Boolean] = BooleanSetting("-Yprint-debug-owners", "when printing trees, print owners of definitions.")
  val YshowPrintErrors: Setting[Boolean] = BooleanSetting("-Yshow-print-errors", "don't suppress exceptions thrown during tree printing.")
  val YshowRawQuoteTrees: Setting[Boolean] = BooleanSetting("-Yshow-raw-tree", "don't remove quote artifacts")
  val YtestPickler: Setting[Boolean] = BooleanSetting("-Ytest-pickler", "self-test for pickling functionality; should be used with -Ystop-after:pickler")
  val YcheckReentrant: Setting[Boolean] = BooleanSetting("-Ycheck-reentrant", "check that compiled program does not contain vars that can be accessed from a global root.")
  val YdropComments: Setting[Boolean] = BooleanSetting("-Ydrop-comments", "Drop comments when scanning source files.")
  val YcookComments: Setting[Boolean] = BooleanSetting("-Ycook-comments", "Cook the comments (type check `@usecase`, etc.)")
  val YforceSbtPhases: Setting[Boolean] = BooleanSetting("-Yforce-sbt-phases", "Run the phases used by sbt for incremental compilation (ExtractDependencies and ExtractAPI) even if the compiler is ran outside of sbt, for debugging.")
  val YdumpSbtInc: Setting[Boolean] = BooleanSetting("-Ydump-sbt-inc", "For every compiled foo.scala, output the API representation and dependencies used for sbt incremental compilation in foo.inc, implies -Yforce-sbt-phases.")
  val YcheckAllPatmat: Setting[Boolean] = BooleanSetting("-Ycheck-all-patmat", "Check exhaustivity and redundancy of all pattern matching (used for testing the algorithm)")
  val YretainTrees: Setting[Boolean] = BooleanSetting("-Yretain-trees", "Retain trees for top-level classes, accessible from ClassSymbol#tree")
  val YshowTreeIds: Setting[Boolean] = BooleanSetting("-Yshow-tree-ids", "Uniquely tag all tree nodes in debugging output.")

  val YprofileEnabled: Setting[Boolean] = BooleanSetting("-Yprofile-enabled", "Enable profiling.")
  val YprofileDestination: Setting[String] = StringSetting("-Yprofile-destination", "file", "where to send profiling output - specify a file, default is to the console.", "")
      //.withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileExternalTool: Setting[List[String]] = PhasesSetting("-Yprofile-external-tool", "Enable profiling for a phase using an external tool hook. Generally only useful for a single phase", "typer")
      //.withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileRunGcBetweenPhases: Setting[List[String]] = PhasesSetting("-Yprofile-run-gc", "Run a GC between phases - this allows heap size to be accurate at the expense of more time. Specify a list of phases, or *", "_")
      //.withPostSetHook( _ => YprofileEnabled.value = true )

  // Extremely experimental language features
  val YnoKindPolymorphism: Setting[Boolean] = BooleanSetting("-Yno-kind-polymorphism", "Enable kind polymorphism (see http://dotty.epfl.ch/docs/reference/kind-polymorphism.html). Potentially unsound.")

  /** Area-specific debug output */
  val YexplainLowlevel: Setting[Boolean] = BooleanSetting("-Yexplain-lowlevel", "When explaining type errors, show types at a lower level.")
  val YnoDoubleBindings: Setting[Boolean] = BooleanSetting("-Yno-double-bindings", "Assert no namedtype is bound twice (should be enabled only if program is error-free).")
  val YshowVarBounds: Setting[Boolean] = BooleanSetting("-Yshow-var-bounds", "Print type variables with their bounds")

  val YnoDecodeStacktraces: Setting[Boolean] = BooleanSetting("-Yno-decode-stacktraces", "Show raw StackOverflow stacktraces, instead of decoding them into triggering operations.")

  val YinstrumentClosures: Setting[Boolean] = BooleanSetting("-Yinstrument-closures", "Add instrumentation code that counts closure creations.")
  val YinstrumentAllocations: Setting[Boolean] = BooleanSetting("-Yinstrument-allocations", "Add instrumentation code that counts allocations.")

  /** Dottydoc specific settings */
  val siteRoot: Setting[String] = StringSetting(
    "-siteroot",
    "site root",
    "A directory containing static files from which to generate documentation",
    sys.props("user.dir")
  )


  val projectName: Setting[String] = StringSetting (
    "-project",
    "project title",
    "The name of the project",
    ""
  )

  val projectVersion: Setting[String] = StringSetting (
    "-project-version",
    "project version",
    "The current version of your project",
    ""
  )

  val projectUrl: Setting[String] = StringSetting (
    "-project-url",
    "project repository homepage",
    "The source repository of your project",
    ""
  )

  val wikiSyntax: Setting[Boolean] = BooleanSetting("-Xwiki-syntax", "Retains the Scala2 behavior of using Wiki Syntax in Scaladoc")
}
