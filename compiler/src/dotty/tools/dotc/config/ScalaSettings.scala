package dotty.tools.dotc
package config

import dotty.tools.dotc.core.Contexts._
import dotty.tools.io.{ Directory, PlainDirectory, AbstractFile, JDK9Reflectors }
import PathResolver.Defaults
import rewrites.Rewrites
import Settings.Setting

/** Settings shared by compiler and scala3doc */
trait CommonScalaSettings { self: Settings.SettingGroup =>
  protected def defaultClasspath: String = sys.env.getOrElse("CLASSPATH", ".")

  /** Path related settings */
  val bootclasspath: Setting[String] = PathSetting("-bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath, aliases = List("--boot-class-path"))
  val extdirs: Setting[String] = PathSetting("-extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs, aliases = List("--extension-directories"))
  val javabootclasspath: Setting[String] = PathSetting("-javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath, aliases = List("--java-boot-class-path"))
  val javaextdirs: Setting[String] = PathSetting("-javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs, aliases = List("--java-extension-directories"))
  val sourcepath: Setting[String] = PathSetting("-sourcepath", "Specify location(s) of source files.", Defaults.scalaSourcePath, aliases = List("--source-path"))
  val sourceroot: Setting[String] = PathSetting("-sourceroot", "Specify workspace root directory.", ".")

  val classpath: Setting[String] = PathSetting("-classpath", "Specify where to find user class files.", defaultClasspath, aliases = List("-cp", "--class-path"))
  val outputDir: Setting[AbstractFile] = OutputSetting("-d", "directory|jar", "Destination for generated classfiles.",
    new PlainDirectory(Directory(".")))
  val color: Setting[String] = ChoiceSetting("-color", "mode", "Colored output", List("always", "never"/*, "auto"*/), "always"/* "auto"*/, aliases = List("--color"))
  val verbose: Setting[Boolean] = BooleanSetting("-verbose", "Output messages about what the compiler is doing.", aliases = List("--verbose"))
  val version: Setting[Boolean] = BooleanSetting("-version", "Print product version and exit.", aliases = List("--version"))
  val pageWidth: Setting[Int] = IntSetting("-pagewidth", "Set page width", 80, aliases = List("--page-width"))
  val silentWarnings: Setting[Boolean] = BooleanSetting("-nowarn", "Silence all warnings.", aliases = List("--no-warnings"))

  /** Other settings */
  val encoding: Setting[String] = StringSetting("-encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding, aliases = List("--encoding"))
  val usejavacp: Setting[Boolean] = BooleanSetting("-usejavacp", "Utilize the java.class.path in classpath resolution.", aliases = List("--use-java-class-path"))

  /** Plugin-related setting */
  val plugin: Setting[List[String]]             = MultiStringSetting  ("-Xplugin", "paths", "Load a plugin from each classpath.")
  val disable: Setting[List[String]]            = MultiStringSetting  ("-Xplugin-disable", "plugin", "Disable plugins by name.")
  val require: Setting[List[String]]            = MultiStringSetting  ("-Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val showPlugins: Setting[Boolean]        = BooleanSetting      ("-Xplugin-list", "Print a synopsis of loaded plugins.")
  val pluginsDir: Setting[String]         = StringSetting       ("-Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val pluginOptions: Setting[List[String]]      = MultiStringSetting  ("-P", "plugin:opt", "Pass an option to a plugin, e.g. -P:<plugin>:<opt>")

   /** Doctool specific settings */
  val siteRoot: Setting[String] = StringSetting(
    "-siteroot",
    "site root",
    "A directory containing static files from which to generate documentation.",
    "./docs"
  )


  val projectName: Setting[String] = StringSetting (
    "-project",
    "project title",
    "The name of the project.",
    ""
  )

  val projectVersion: Setting[String] = StringSetting (
    "-project-version",
    "project version",
    "The current version of your project.",
    ""
  )

  val projectLogo: Setting[String] = StringSetting(
    "-project-logo",
    "project logo filename",
    "The file that contains the project's logo (in /images).",
    ""
  )
}

class ScalaSettings extends Settings.SettingGroup with CommonScalaSettings {
  private val minTargetVersion = 8
  private val maxTargetVersion = 15

  private def supportedTargetVersions: List[String] =
    (minTargetVersion to maxTargetVersion).toList.map(_.toString)

  protected def supportedReleaseVersions: List[String] =
    if scala.util.Properties.isJavaAtLeast("9") then
      val jdkVersion = JDK9Reflectors.runtimeVersionMajor(JDK9Reflectors.runtimeVersion()).intValue()
      val maxVersion = Math.min(jdkVersion, maxTargetVersion)
      (minTargetVersion to maxVersion).toList.map(_.toString)
    else List()

  /** Path related settings */
  val semanticdbTarget: Setting[String] = PathSetting("-semanticdb-target", "Specify an alternative output directory for SemanticDB files.", "")

  val deprecation: Setting[Boolean] = BooleanSetting("-deprecation", "Emit warning and location for usages of deprecated APIs.", aliases = List("--deprecation"))
  val explainTypes: Setting[Boolean] = BooleanSetting("-explain-types", "Explain type errors in more detail.", aliases = List("--explain-types"))
  val explain: Setting[Boolean] = BooleanSetting("-explain", "Explain errors in more detail.", aliases = List("--explain"))
  val feature: Setting[Boolean] = BooleanSetting("-feature", "Emit warning and location for usages of features that should be imported explicitly.", aliases = List("--feature"))
  val help: Setting[Boolean] = BooleanSetting("-help", "Print a synopsis of standard options.", aliases = List("--help"))
  val release: Setting[String] = ChoiceSetting("-release", "release", "Compile code with classes specific to the given version of the Java platform available on the classpath and emit bytecode for this version.", supportedReleaseVersions, "", aliases = List("--release"))
  val source: Setting[String] = ChoiceSetting("-source", "source version", "source version", List("3.0", "3.1", "3.0-migration", "3.1-migration"), "3.0", aliases = List("--source"))
  val scalajs: Setting[Boolean] = BooleanSetting("-scalajs", "Compile in Scala.js mode (requires scalajs-library.jar on the classpath).", aliases = List("--scalajs"))
  val unchecked: Setting[Boolean] = BooleanSetting("-unchecked", "Enable additional warnings where generated code depends on assumptions.", aliases = List("--unchecked"))
  val uniqid: Setting[Boolean] = BooleanSetting("-uniqid", "Uniquely tag all identifiers in debugging output.", aliases = List("--unique-id"))
  val language: Setting[List[String]] = MultiStringSetting("-language", "feature", "Enable one or more language features.", aliases = List("--language"))
  val rewrite: Setting[Option[Rewrites]] = OptionSetting[Rewrites]("-rewrite", "When used in conjunction with a `...-migration` source version, rewrites sources to migrate to new version.", aliases = List("--rewrite"))
  val fromTasty: Setting[Boolean] = BooleanSetting("-from-tasty", "Compile classes from tasty files. The arguments are .tasty or .jar files.", aliases = List("--from-tasty"))

  val newSyntax: Setting[Boolean] = BooleanSetting("-new-syntax", "Require `then` and `do` in control expressions.")
  val oldSyntax: Setting[Boolean] = BooleanSetting("-old-syntax", "Require `(...)` around conditions.")
  val indent: Setting[Boolean] = BooleanSetting("-indent", "Together with -rewrite, remove {...} syntax when possible due to significant indentation.")
  val noindent: Setting[Boolean] = BooleanSetting("-noindent", "Require classical {...} syntax, indentation is not significant.")
  val YindentColons: Setting[Boolean] = BooleanSetting("-Yindent-colons", "Allow colons at ends-of-lines to start indentation blocks.")

  /** Decompiler settings */
  val printTasty: Setting[Boolean] = BooleanSetting("-print-tasty", "Prints the raw tasty.", aliases = List("--print-tasty"))
  val printLines: Setting[Boolean] = BooleanSetting("-print-lines", "Show source code line numbers.", aliases = List("--print-lines"))

  /** Scala.js-related settings */
  val scalajsGenStaticForwardersForNonTopLevelObjects: Setting[Boolean] = BooleanSetting("-scalajs-genStaticForwardersForNonTopLevelObjects", "Generate static forwarders even for non-top-level objects (Scala.js only)")
  val scalajsMapSourceURI: Setting[List[String]] = MultiStringSetting("-scalajs-mapSourceURI", "uri1[->uri2]", "rebases source URIs from uri1 to uri2 (or to a relative URI) for source maps (Scala.js only)")

  /** -X "Advanced" settings */
  val Xhelp: Setting[Boolean] = BooleanSetting("-X", "Print a synopsis of advanced options.")
  val XnoForwarders: Setting[Boolean] = BooleanSetting("-Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val XmaxInlines: Setting[Int] = IntSetting("-Xmax-inlines", "Maximal number of successive inlines.", 32)
  val XmaxInlinedTrees: Setting[Int] = IntSetting("-Xmax-inlined-trees", "Maximal number of inlined trees.", 2_000_000)
  val Xmigration: Setting[ScalaVersion] = VersionSetting("-Xmigration", "Warn about constructs whose behavior may have changed since version.")
  val Xprint: Setting[List[String]] = PhasesSetting("-Xprint", "Print out program after")
  val XprintTypes: Setting[Boolean] = BooleanSetting("-Xprint-types", "Print tree types (debugging option).")
  val XprintDiff: Setting[Boolean] = BooleanSetting("-Xprint-diff", "Print changed parts of the tree since last print.")
  val XprintDiffDel: Setting[Boolean] = BooleanSetting("-Xprint-diff-del", "Print changed parts of the tree since last print including deleted parts.")
  val XprintInline: Setting[Boolean] = BooleanSetting("-Xprint-inline", "Show where inlined code comes from.")
  val XprintSuspension: Setting[Boolean] = BooleanSetting("-Xprint-suspension", "Show when code is suspended until macros are compiled.")
  val Xprompt: Setting[Boolean] = BooleanSetting("-Xprompt", "Display a prompt after each error (debugging option).")
  val XshowPhases: Setting[Boolean] = BooleanSetting("-Xshow-phases", "Print all compiler phases.")
  val XreplDisableDisplay: Setting[Boolean] = BooleanSetting("-Xrepl-disable-display", "Do not display definitions in REPL.")
  val XfatalWarnings: Setting[Boolean] = BooleanSetting("-Xfatal-warnings", "Fail the compilation if there are any warnings.")
  val XverifySignatures: Setting[Boolean] = BooleanSetting("-Xverify-signatures", "Verify generic signatures in generated bytecode.")
  val XignoreScala2Macros: Setting[Boolean] = BooleanSetting("-Xignore-scala2-macros", "Ignore errors when compiling code that calls Scala2 macros, these will fail at runtime.")
  val XimportSuggestionTimeout: Setting[Int] = IntSetting("-Ximport-suggestion-timeout", "Timeout (in ms) for searching for import suggestions when errors are reported.", 8000)
  val Xsemanticdb: Setting[Boolean] = BooleanSetting("-Xsemanticdb", "Store information in SemanticDB.", aliases = List("-Ysemanticdb"))
  val Xtarget: Setting[String] = ChoiceSetting("-Xtarget", "target", "Emit bytecode for the specified version of the Java platform. This might produce bytecode that will break at runtime. When on JDK 9+, consider -release as a safer alternative.", supportedTargetVersions, "", aliases = List("--Xtarget"))

  val XmixinForceForwarders = ChoiceSetting(
    name    = "-Xmixin-force-forwarders",
    helpArg = "mode",
    descr   = "Generate forwarder methods in classes inhering concrete methods from traits.",
    choices = List("true", "junit", "false"),
    default = "true")

  object mixinForwarderChoices {
    def isTruthy(using Context) = XmixinForceForwarders.value == "true"
    def isAtLeastJunit(using Context) = isTruthy || XmixinForceForwarders.value == "junit"
  }

  /** -Y "Private" settings */
  val Yhelp: Setting[Boolean] = BooleanSetting("-Y", "Print a synopsis of private options.")
  val Ycheck: Setting[List[String]] = PhasesSetting("-Ycheck", "Check the tree at the end of")
  val YcheckMods: Setting[Boolean] = BooleanSetting("-Ycheck-mods", "Check that symbols and their defining trees have modifiers in sync.")
  val Ydebug: Setting[Boolean] = BooleanSetting("-Ydebug", "Increase the quantity of debugging output.")
  val YdebugTrace: Setting[Boolean] = BooleanSetting("-Ydebug-trace", "Trace core operations.")
  val YdebugFlags: Setting[Boolean] = BooleanSetting("-Ydebug-flags", "Print all flags of definitions.")
  val YdebugMissingRefs: Setting[Boolean] = BooleanSetting("-Ydebug-missing-refs", "Print a stacktrace when a required symbol is missing.")
  val YdebugNames: Setting[Boolean] = BooleanSetting("-Ydebug-names", "Show internal representation of names.")
  val YdebugPos: Setting[Boolean] = BooleanSetting("-Ydebug-pos", "Show full source positions including spans.")
  val YdebugTreeWithId: Setting[Int] = IntSetting("-Ydebug-tree-with-id", "Print the stack trace when the tree with the given id is created.", Int.MinValue)
  val YdebugTypeError: Setting[Boolean] = BooleanSetting("-Ydebug-type-error", "Print the stack trace when a TypeError is caught", false)
  val YdebugError: Setting[Boolean] = BooleanSetting("-Ydebug-error", "Print the stack trace when any error is caught.", false)
  val YtermConflict: Setting[String] = ChoiceSetting("-Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val Ylog: Setting[List[String]] = PhasesSetting("-Ylog", "Log operations during")
  val YlogClasspath: Setting[Boolean] = BooleanSetting("-Ylog-classpath", "Output information about what classpath is being applied.")
  val YdisableFlatCpCaching: Setting[Boolean] = BooleanSetting("-YdisableFlatCpCaching", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.")

  val Yscala2Unpickler: Setting[String] = StringSetting("-Yscala2-unpickler", "", "Control where we may get Scala 2 symbols from. This is either \"always\", \"never\", or a classpath.", "always")

  val YnoImports: Setting[Boolean] = BooleanSetting("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val YnoGenericSig: Setting[Boolean] = BooleanSetting("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val YnoPredef: Setting[Boolean] = BooleanSetting("-Yno-predef", "Compile without importing Predef.")
  val Yskip: Setting[List[String]] = PhasesSetting("-Yskip", "Skip")
  val Ydumpclasses: Setting[String] = StringSetting("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val YstopAfter: Setting[List[String]] = PhasesSetting("-Ystop-after", "Stop after", aliases = List("-stop")) // backward compat
  val YstopBefore: Setting[List[String]] = PhasesSetting("-Ystop-before", "Stop before") // stop before erasure as long as we have not debugged it fully
  val YshowSuppressedErrors: Setting[Boolean] = BooleanSetting("-Yshow-suppressed-errors", "Also show follow-on errors and warnings that are normally suppressed.")
  val YdetailedStats: Setting[Boolean] = BooleanSetting("-Ydetailed-stats", "Show detailed internal compiler stats (needs Stats.enabled to be set to true).")
  val YkindProjector: Setting[Boolean] = BooleanSetting("-Ykind-projector", "Allow `*` as wildcard to be compatible with kind projector.")
  val YprintPos: Setting[Boolean] = BooleanSetting("-Yprint-pos", "Show tree positions.")
  val YprintPosSyms: Setting[Boolean] = BooleanSetting("-Yprint-pos-syms", "Show symbol definitions positions.")
  val YnoDeepSubtypes: Setting[Boolean] = BooleanSetting("-Yno-deep-subtypes", "Throw an exception on deep subtyping call stacks.")
  val YnoPatmatOpt: Setting[Boolean] = BooleanSetting("-Yno-patmat-opt", "Disable all pattern matching optimizations.")
  val YplainPrinter: Setting[Boolean] = BooleanSetting("-Yplain-printer", "Pretty-print using a plain printer.")
  val YprintSyms: Setting[Boolean] = BooleanSetting("-Yprint-syms", "When printing trees print info in symbols instead of corresponding info in trees.")
  val YprintDebug: Setting[Boolean] = BooleanSetting("-Yprint-debug", "When printing trees, print some extra information useful for debugging.")
  val YprintDebugOwners: Setting[Boolean] = BooleanSetting("-Yprint-debug-owners", "When printing trees, print owners of definitions.")
  val YshowPrintErrors: Setting[Boolean] = BooleanSetting("-Yshow-print-errors", "Don't suppress exceptions thrown during tree printing.")
  val YtestPickler: Setting[Boolean] = BooleanSetting("-Ytest-pickler", "Self-test for pickling functionality; should be used with -Ystop-after:pickler.")
  val YcheckReentrant: Setting[Boolean] = BooleanSetting("-Ycheck-reentrant", "Check that compiled program does not contain vars that can be accessed from a global root.")
  val YdropComments: Setting[Boolean] = BooleanSetting("-Ydrop-comments", "Drop comments when scanning source files.")
  val YcookComments: Setting[Boolean] = BooleanSetting("-Ycook-comments", "Cook the comments (type check `@usecase`, etc.)")
  val YforceSbtPhases: Setting[Boolean] = BooleanSetting("-Yforce-sbt-phases", "Run the phases used by sbt for incremental compilation (ExtractDependencies and ExtractAPI) even if the compiler is ran outside of sbt, for debugging.")
  val YdumpSbtInc: Setting[Boolean] = BooleanSetting("-Ydump-sbt-inc", "For every compiled foo.scala, output the API representation and dependencies used for sbt incremental compilation in foo.inc, implies -Yforce-sbt-phases.")
  val YcheckAllPatmat: Setting[Boolean] = BooleanSetting("-Ycheck-all-patmat", "Check exhaustivity and redundancy of all pattern matching (used for testing the algorithm).")
  val YretainTrees: Setting[Boolean] = BooleanSetting("-Yretain-trees", "Retain trees for top-level classes, accessible from ClassSymbol#tree")
  val YshowTreeIds: Setting[Boolean] = BooleanSetting("-Yshow-tree-ids", "Uniquely tag all tree nodes in debugging output.")
  val YfromTastyIgnoreList: Setting[List[String]] = MultiStringSetting("-Yfrom-tasty-ignore-list", "file", "List of `tasty` files in jar files that will not be loaded when using -from-tasty")

  val YprofileEnabled: Setting[Boolean] = BooleanSetting("-Yprofile-enabled", "Enable profiling.")
  val YprofileDestination: Setting[String] = StringSetting("-Yprofile-destination", "file", "Where to send profiling output - specify a file, default is to the console.", "")
      //.withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileExternalTool: Setting[List[String]] = PhasesSetting("-Yprofile-external-tool", "Enable profiling for a phase using an external tool hook. Generally only useful for a single phase.", "typer")
      //.withPostSetHook( _ => YprofileEnabled.value = true )
  val YprofileRunGcBetweenPhases: Setting[List[String]] = PhasesSetting("-Yprofile-run-gc", "Run a GC between phases - this allows heap size to be accurate at the expense of more time. Specify a list of phases, or *", "_")
      //.withPostSetHook( _ => YprofileEnabled.value = true )

  // Extremely experimental language features
  val YnoKindPolymorphism: Setting[Boolean] = BooleanSetting("-Yno-kind-polymorphism", "Disable kind polymorphism.")
  val YexplicitNulls: Setting[Boolean] = BooleanSetting("-Yexplicit-nulls", "Make reference types non-nullable. Nullable types can be expressed with unions: e.g. String|Null.")
  val YerasedTerms: Setting[Boolean] = BooleanSetting("-Yerased-terms", "Allows the use of erased terms.")
  val YcheckInit: Setting[Boolean] = BooleanSetting("-Ycheck-init", "Check initialization of objects")
  val YrequireTargetName: Setting[Boolean] = BooleanSetting("-Yrequire-targetName", "Warn if an operator is defined without a @targetName annotation")
  val YunsoundMatchTypes: Setting[Boolean] = BooleanSetting("-Yunsound-match-types", "Use unsound match type reduction algorithm.")

  /** Area-specific debug output */
  val YexplainLowlevel: Setting[Boolean] = BooleanSetting("-Yexplain-lowlevel", "When explaining type errors, show types at a lower level.")
  val YnoDoubleBindings: Setting[Boolean] = BooleanSetting("-Yno-double-bindings", "Assert no namedtype is bound twice (should be enabled only if program is error-free).")
  val YshowVarBounds: Setting[Boolean] = BooleanSetting("-Yshow-var-bounds", "Print type variables with their bounds.")

  val YnoDecodeStacktraces: Setting[Boolean] = BooleanSetting("-Yno-decode-stacktraces", "Show raw StackOverflow stacktraces, instead of decoding them into triggering operations.")

  val Yinstrument: Setting[Boolean] = BooleanSetting("-Yinstrument", "Add instrumentation code that counts allocations and closure creations.")
  val YinstrumentDefs: Setting[Boolean] = BooleanSetting("-Yinstrument-defs", "Add instrumentation code that counts method calls; needs -Yinstrument to be set, too.")

  /** Dottydoc specific settings that are not used in scala3doc */
  val docSnapshot: Setting[Boolean] = BooleanSetting("-doc-snapshot", "Generate a documentation snapshot for the current Dotty version")

  val projectUrl: Setting[String] = StringSetting (
    "-project-url",
    "project repository homepage",
    "The source repository of your project.",
    ""
  )

  val wikiSyntax: Setting[Boolean] = BooleanSetting("-Xwiki-syntax", "Retains the Scala2 behavior of using Wiki Syntax in Scaladoc.")
}
