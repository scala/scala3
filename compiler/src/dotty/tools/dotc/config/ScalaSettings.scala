package dotty.tools.dotc
package config

import dotty.tools.dotc.config.PathResolver.Defaults
import dotty.tools.dotc.config.Settings.{Setting, SettingAlias, SettingGroup, SettingCategory, Deprecation}
import dotty.tools.dotc.config.SourceVersion
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.rewrites.Rewrites
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, NoAbstractFile}
import Setting.ChoiceWithHelp
import ScalaSettingCategories.*

import dotty.tools.dotc.util.chaining.*

import java.util.zip.Deflater

enum ScalaSettingCategories(val prefixLetter: String) extends SettingCategory:
  // Root settings, a category for setting that are used to configure the core compilation process
  case RootSetting extends ScalaSettingCategories("")
  // Warning settings, a category for settings that are used to enable and configure warnings
  case WarningSetting extends ScalaSettingCategories("W")
  // Fork / private settings, a category for settings that enable private or advanced features, mainly used for debugging the compiler
  case ForkSetting extends ScalaSettingCategories("Y")
  // Advanced settings, a category for settings that enable advanced, often unstable, features
  case AdvancedSetting extends ScalaSettingCategories("X")
  // Verbose settings, a category to configure the verbosity of the compiler
  case VerboseSetting extends ScalaSettingCategories("V")

object ScalaSettings extends ScalaSettings

// Kept as seperate type to avoid breaking backward compatibility
abstract class ScalaSettings extends SettingGroup, AllScalaSettings:
  val settingsByCategory: Map[SettingCategory, List[Setting[?]]] =
    allSettings.groupBy(_.category)
      .view.mapValues(_.toList).toMap
      .withDefaultValue(Nil)
  def categories: List[SettingCategory] = settingsByCategory.keys.toList.sortBy(_.prefixLetter)
  val rootSettings: List[Setting[?]] = settingsByCategory(RootSetting).sortBy(_.name)
  val warningSettings: List[Setting[?]] = settingsByCategory(WarningSetting).sortBy(_.name)
  val forkSettings: List[Setting[?]] = settingsByCategory(ForkSetting).sortBy(_.name)
  val advancedSettings: List[Setting[?]] = settingsByCategory(AdvancedSetting).sortBy(_.name)
  val verboseSettings: List[Setting[?]] = settingsByCategory(VerboseSetting).sortBy(_.name)
  val settingsByAliases: Map[String, Setting[?]] = allSettings.flatMap(s => s.aliases.map(_.name -> s)).toMap


trait AllScalaSettings extends CommonScalaSettings, PluginSettings, VerboseSettings, WarningSettings, XSettings, YSettings:
  self: SettingGroup =>

  /* Path related settings */
  val semanticdbTarget: Setting[String] = PathSetting(RootSetting, "semanticdb-target", "Specify an alternative output directory for SemanticDB files.", "")
  val semanticdbText: Setting[Boolean] = BooleanSetting(RootSetting, "semanticdb-text", "Specifies whether to include source code in SemanticDB files or not.")

  val source: Setting[String] = ChoiceSetting(RootSetting, "source", "source version", "source version", ScalaSettingsProperties.supportedSourceVersions, SourceVersion.defaultSourceVersion.toString, aliases = List("--source"))
  val uniqid: Setting[Boolean] = BooleanSetting(RootSetting, "uniqid", "Uniquely tag all identifiers in debugging output.", aliases = List("--unique-id"))
  val rewrite: Setting[Option[Rewrites]] = OptionSetting[Rewrites](RootSetting, "rewrite", "When used in conjunction with a `...-migration` source version, rewrites sources to migrate to new version.", aliases = List("--rewrite"))
  val fromTasty: Setting[Boolean] = BooleanSetting(RootSetting, "from-tasty", "Compile classes from tasty files. The arguments are .tasty or .jar files.", aliases = List("--from-tasty"))

  val newSyntax: Setting[Boolean] = BooleanSetting(RootSetting, "new-syntax", "Require `then` and `do` in control expressions.")
  val oldSyntax: Setting[Boolean] = BooleanSetting(RootSetting, "old-syntax", "Require `(...)` around conditions.")
  val indent: Setting[Boolean] = BooleanSetting(RootSetting, "indent", "Together with -rewrite, remove {...} syntax when possible due to significant indentation.")
  val noindent: Setting[Boolean] = BooleanSetting(RootSetting, "no-indent", "Require classical {...} syntax, indentation is not significant.", aliases = List("-noindent"))

  /* Decompiler settings */
  val printTasty: Setting[Boolean] = BooleanSetting(RootSetting, "print-tasty", "Prints the raw tasty.", aliases = List("--print-tasty"))

  /* Scala.js-related settings */
  val scalajsGenStaticForwardersForNonTopLevelObjects: Setting[Boolean] = BooleanSetting(RootSetting, "scalajs-genStaticForwardersForNonTopLevelObjects", "Generate static forwarders even for non-top-level objects (Scala.js only).")
  val scalajsMapSourceURI: Setting[List[String]] = MultiStringSetting(RootSetting, "scalajs-mapSourceURI", "uri1[->uri2]", "rebases source URIs from uri1 to uri2 (or to a relative URI) for source maps (Scala.js only).")

  val projectUrl: Setting[String] = StringSetting (
    RootSetting,
    "project-url",
    "project repository homepage",
    "The source repository of your project.",
    ""
  )

  val wikiSyntax: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xwiki-syntax", "Retains the Scala2 behavior of using Wiki Syntax in Scaladoc.")
  val jvmargs  = PrefixSetting(RootSetting, "J<flag>", "Pass -J<flag> directly to the runtime system.")
  val defines  = PrefixSetting(RootSetting, "D<property=value>", "Pass -D<property=value> directly to the runtime system.")
end AllScalaSettings

/** Settings shared by compiler and scaladoc */
trait CommonScalaSettings:
  self: SettingGroup =>

  /* Path related settings */
  val bootclasspath: Setting[String] = PathSetting(RootSetting, "bootclasspath", "Override location of bootstrap class files.", Defaults.scalaBootClassPath, aliases = List("--boot-class-path"))
  val extdirs: Setting[String] = PathSetting(RootSetting, "extdirs", "Override location of installed extensions.", Defaults.scalaExtDirs, aliases = List("--extension-directories"))
  val javabootclasspath: Setting[String] = PathSetting(RootSetting, "javabootclasspath", "Override java boot classpath.", Defaults.javaBootClassPath, aliases = List("--java-boot-class-path"))
  val javaextdirs: Setting[String] = PathSetting(RootSetting, "javaextdirs", "Override java extdirs classpath.", Defaults.javaExtDirs, aliases = List("--java-extension-directories"))
  val sourcepath: Setting[String] = PathSetting(RootSetting, "sourcepath", "Specify location(s) of source files.", Defaults.scalaSourcePath, aliases = List("--source-path"))
  val sourceroot: Setting[String] = PathSetting(RootSetting, "sourceroot", "Specify workspace root directory.", ".")

  val classpath: Setting[String] = PathSetting(RootSetting, "classpath", "Specify where to find user class files.", ScalaSettingsProperties.defaultClasspath, aliases = List("-cp", "--class-path"))
  val outputDir: Setting[AbstractFile] = OutputSetting(RootSetting, "d", "directory|jar", "Destination for generated classfiles.",
    new PlainDirectory(Directory(".")))
  val color: Setting[String] = ChoiceSetting(RootSetting, "color", "mode", "Colored output", List("always", "never"/*, "auto"*/), "always"/* "auto"*/, aliases = List("--color"))
  val verbose: Setting[Boolean] = BooleanSetting(RootSetting, "verbose", "Output messages about what the compiler is doing.", aliases = List("--verbose"))
  val version: Setting[Boolean] = BooleanSetting(RootSetting, "version", "Print product version and exit.", aliases = List("--version"))
  val help: Setting[Boolean] = BooleanSetting(RootSetting, "help", "Print a synopsis of standard options.", aliases = List("--help", "-h"))
  val pageWidth: Setting[Int] = IntSetting(RootSetting, "pagewidth", "Set page width", ScalaSettingsProperties.defaultPageWidth, aliases = List("--page-width"))
  val silentWarnings: Setting[Boolean] = BooleanSetting(RootSetting, "nowarn", "Silence all warnings.", aliases = List("--no-warnings"))

  val javaOutputVersion: Setting[String] = ChoiceSetting(RootSetting, "java-output-version", "version", "Compile code with classes specific to the given version of the Java platform available on the classpath and emit bytecode for this version. Corresponds to -release flag in javac.", ScalaSettingsProperties.supportedReleaseVersions, "", aliases = List("-release", "--release"))

  val deprecation: Setting[Boolean] = BooleanSetting(RootSetting, "deprecation", "Emit warning and location for usages of deprecated APIs.", aliases = List("--deprecation"))
  val feature: Setting[Boolean] = BooleanSetting(RootSetting, "feature", "Emit warning and location for usages of features that should be imported explicitly.", aliases = List("--feature"))
  val explain: Setting[Boolean] = BooleanSetting(RootSetting, "explain", "Explain errors in more detail.", aliases = List("--explain"))
  // -explain-types setting is necessary for cross compilation, since it is mentioned in sbt-tpolecat, for instance
  // it is otherwise subsumed by -explain, and should be dropped as soon as we can.
  val explainTypes: Setting[Boolean] = BooleanSetting(RootSetting, "explain-types", "Explain type errors in more detail (deprecated, use -explain instead).", aliases = List("--explain-types", "-explaintypes"))
  val explainCyclic: Setting[Boolean] = BooleanSetting(RootSetting, "explain-cyclic", "Explain cyclic reference errors in more detail.", aliases = List("--explain-cyclic"))
  val unchecked: Setting[Boolean] = BooleanSetting(RootSetting, "unchecked", "Enable additional warnings where generated code depends on assumptions.", initialValue = true, aliases = List("--unchecked"))
  val language: Setting[List[ChoiceWithHelp[String]]] = MultiChoiceHelpSetting(RootSetting, "language", "feature", "Enable one or more language features.", choices = ScalaSettingsProperties.supportedLanguageFeatures, legacyChoices = ScalaSettingsProperties.legacyLanguageFeatures, default = Nil, aliases = List("--language"))
  val experimental: Setting[Boolean] = BooleanSetting(RootSetting, "experimental", "Annotate all top-level definitions with @experimental. This enables the use of experimental features anywhere in the project.")
  val preview: Setting[Boolean] = BooleanSetting(RootSetting, "preview", "Enable the use of preview features anywhere in the project.")

  /* Coverage settings */
  val coverageOutputDir = PathSetting(RootSetting, "coverage-out", "Destination for coverage classfiles and instrumentation data.", "", aliases = List("--coverage-out"))
  val coverageExcludeClasslikes: Setting[List[String]] = MultiStringSetting(RootSetting, "coverage-exclude-classlikes", "packages, classes and modules", "List of regexes for packages, classes and modules to exclude from coverage.", aliases = List("--coverage-exclude-classlikes"))
  val coverageExcludeFiles: Setting[List[String]] = MultiStringSetting(RootSetting, "coverage-exclude-files", "files", "List of regexes for files to exclude from coverage.", aliases = List("--coverage-exclude-files"))

  /* Other settings */
  val encoding: Setting[String] = StringSetting(RootSetting, "encoding", "encoding", "Specify character encoding used by source files.", Properties.sourceEncoding, aliases = List("--encoding"))
  val scalajs: Setting[Boolean] = BooleanSetting(RootSetting, "scalajs", "Compile in Scala.js mode (requires scalajs-library.jar on the classpath).", aliases = List("--scalajs"))
  val replInitScript: Setting[String] = StringSetting(RootSetting, "repl-init-script", "code", "The code will be run on REPL startup.", "", aliases = List("--repl-init-script"))
  val replQuitAfterInit: Setting[Boolean] = BooleanSetting(RootSetting, "repl-quit-after-init", "Quit REPL after evaluating the init script.", aliases = List("--repl-quit-after-init"))

  /* YSettings shared with scaladoc */
  val Yusejavacp: Setting[Boolean] = BooleanSetting(ForkSetting, "Yusejavacp", "Utilize the java.class.path in classpath resolution.", aliases = List("--use-java-class-path", "-usejavacp", "--usejavacp"))
end CommonScalaSettings

/** -P "plugin" settings. Various tools might support plugins. */
private sealed trait PluginSettings:
  self: SettingGroup =>
  val plugin: Setting[List[String]]        = MultiStringSetting  (AdvancedSetting, "Xplugin", "paths", "Load a plugin from each classpath.")
  val disable: Setting[List[String]]       = MultiStringSetting  (AdvancedSetting, "Xplugin-disable", "plugin", "Disable plugins by name.")
  val require: Setting[List[String]]       = MultiStringSetting  (AdvancedSetting, "Xplugin-require", "plugin", "Abort if a named plugin is not loaded.")
  val showPlugins: Setting[Boolean]        = BooleanSetting      (AdvancedSetting, "Xplugin-list", "Print a synopsis of loaded plugins.")
  val pluginsDir: Setting[String]          = StringSetting       (AdvancedSetting, "Xpluginsdir", "path", "Path to search for plugin archives.", Defaults.scalaPluginPath)
  val pluginOptions: Setting[List[String]] = MultiStringSetting  (RootSetting, "P", "plugin:opt", "Pass an option to a plugin, e.g. -P:<plugin>:<opt>")

/** -V "Verbose" settings */
private sealed trait VerboseSettings:
  self: SettingGroup =>
  val Vhelp: Setting[Boolean] = BooleanSetting(VerboseSetting, "V", "Print a synopsis of verbose options.")
  val Vprint: Setting[List[String]] = PhasesSetting(VerboseSetting, "Vprint", "Print out program after", aliases = SettingAlias("-Xprint", Deprecation()) :: Nil)
  val XshowPhases: Setting[Boolean] = BooleanSetting(VerboseSetting, "Vphases", "List compiler phases.", aliases = SettingAlias("-Xshow-phases", Deprecation()) :: Nil)

  val Vprofile: Setting[Boolean] = BooleanSetting(VerboseSetting, "Vprofile", "Show metrics about sources and internal representations to estimate compile-time complexity.")
  val VprofileSortedBy = ChoiceSetting(VerboseSetting, "Vprofile-sorted-by", "key", "Show metrics about sources and internal representations sorted by given column name", List("name", "path", "lines", "tokens", "tasty", "complexity"), "")
  val VprofileDetails = IntSetting(VerboseSetting, "Vprofile-details", "Show metrics about sources and internal representations of the most complex methods", 0)
  val VreplMaxPrintElements: Setting[Int] = IntSetting(VerboseSetting, "Vrepl-max-print-elements", "Number of elements to be printed before output is truncated.", 1000)
  val VreplMaxPrintCharacters: Setting[Int] = IntSetting(VerboseSetting, "Vrepl-max-print-characters", "Number of characters to be printed before output is truncated.", 50000)

/** -W "Warnings" settings
 */
private sealed trait WarningSettings:
  self: SettingGroup =>

  val Whelp: Setting[Boolean] = BooleanSetting(WarningSetting, "W", "Print a synopsis of warning options.")
  val Werror: Setting[Boolean] = BooleanSetting(WarningSetting, "Werror", "Fail the compilation if there are any warnings.", aliases = SettingAlias("-Xfatal-warnings", Deprecation()) :: Nil)
  val Wall: Setting[Boolean] = BooleanSetting(WarningSetting, "Wall", "Enable all warning settings.")
  private val WvalueDiscard: Setting[Boolean] = BooleanSetting(WarningSetting, "Wvalue-discard", "Warn when non-Unit expression results are unused.")
  private val WNonUnitStatement = BooleanSetting(WarningSetting, "Wnonunit-statement", "Warn when block statements are non-Unit expressions.")
  private val WenumCommentDiscard = BooleanSetting(WarningSetting, "Wenum-comment-discard", "Warn when a comment ambiguously assigned to multiple enum cases is discarded.")
  private val WimplausiblePatterns = BooleanSetting(WarningSetting, "Wimplausible-patterns", "Warn if comparison with a pattern value looks like it might always fail.")
  private val WunstableInlineAccessors = BooleanSetting(WarningSetting, "WunstableInlineAccessors", "Warn an inline methods has references to non-stable binary APIs.")
  private val WtoStringInterpolated = BooleanSetting(WarningSetting, "Wtostring-interpolated", "Warn a standard interpolator used toString on a reference type.")
  private val WrecurseWithDefault = BooleanSetting(WarningSetting, "Wrecurse-with-default", "Warn when a method calls itself with a default argument.")
  private val WwrongArrow = BooleanSetting(WarningSetting, "Wwrong-arrow", "Warn if function arrow was used instead of context literal ?=>.")
  private val WinferUnion = BooleanSetting(WarningSetting, "Winfer-union", "Warn if type argument was inferred as union type.")
  private val Wunused: Setting[List[ChoiceWithHelp[String]]] = MultiChoiceHelpSetting(
    WarningSetting,
    name = "Wunused",
    helpArg = "warning",
    descr = "Enable or disable specific `unused` warnings",
    choices = List(
      ChoiceWithHelp("nowarn", ""),
      ChoiceWithHelp("all", ""),
      ChoiceWithHelp("imports", "Warn if an import selector is not referenced."),
      ChoiceWithHelp("privates", "Warn if a private member is unused"),
      ChoiceWithHelp("locals", "Warn if a local definition is unused"),
      ChoiceWithHelp("explicits", "Warn if an explicit parameter is unused"),
      ChoiceWithHelp("implicits", "Warn if an implicit parameter is unused"),
      ChoiceWithHelp("params", "Enable -Wunused:explicits,implicits"),
      ChoiceWithHelp("patvars","Warn if a variable bound in a pattern is unused"),
      //ChoiceWithHelp("inlined", "Apply -Wunused to inlined expansions"), // TODO
      ChoiceWithHelp("linted", "Enable -Wunused:imports,privates,locals,implicits"),
    ),
    default = Nil
  )
  object WunusedHas:
    def isChoiceSet(s: String)(using Context) = Wunused.value.pipe(us => us.contains(s))
    def allOr(s: String)(using Context) = Wall.value || Wunused.value.pipe(us => us.contains("all") || us.contains(s))
    def nowarn(using Context) = allOr("nowarn")

    // Is any choice set for -Wunused?
    def any(using Context): Boolean = Wall.value || Wunused.value.nonEmpty

    def imports(using Context) =
      (allOr("imports") || allOr("linted")) && !(strictNoImplicitWarn)
    def locals(using Context) =
      allOr("locals") || allOr("linted")
    /** -Wunused:explicits OR -Wunused:params */
    def explicits(using Context) =
      allOr("explicits") || allOr("params")
    /** -Wunused:implicits OR -Wunused:params */
    def implicits(using Context) =
      allOr("implicits") || allOr("params") || allOr("linted")
    def params(using Context) = allOr("params")
    def privates(using Context) =
      allOr("privates") || allOr("linted")
    def patvars(using Context) = allOr("patvars") || isChoiceSet("unsafe-warn-patvars")
    def inlined(using Context) = isChoiceSet("inlined")
    def linted(using Context) =
      allOr("linted")
    def strictNoImplicitWarn(using Context) =
      isChoiceSet("strict-no-implicit-warn")

  val Wconf: Setting[List[String]] = MultiStringSetting(
    WarningSetting,
    "Wconf",
    "patterns",
    default = List(),
    descr =
    raw"""Configure compiler warnings.
         |Syntax: -Wconf:<filters>:<action>,<filters>:<action>,...
         |multiple <filters> are combined with &, i.e., <filter>&...&<filter>
         |
         |<filter>
         |  - Any message: any
         |
         |  - Message categories: cat=deprecation, cat=feature, cat=unchecked
         |
         |  - Message content: msg=regex
         |    The regex need only match some part of the message, not all of it.
         |
         |  - Message id: id=E129
         |    The message id is printed with the warning.
         |
         |  - Message name: name=PureExpressionInStatementPosition
         |    The message name is printed with the warning in verbose warning mode.
         |
         |  - Source location: src=regex
         |    The regex is evaluated against the full source path.
         |
         |  - Origin of warning: origin=regex
         |    The regex must match the full name (`package.Class.method`) of the deprecated entity.
         |
         |In verbose warning mode the compiler prints matching filters for warnings.
         |Verbose mode can be enabled globally using `-Wconf:any:verbose`, or locally
         |using the @nowarn annotation (example: `@nowarn("v") def test = try 1`).
         |
         |<action>
         |  - error / e
         |  - warning / w
         |  - verbose / v (emit warning, show additional help for writing `-Wconf` filters)
         |  - info / i    (infos are not counted as warnings and not affected by `-Werror`)
         |  - silent / s
         |
         |The default configuration is empty.
         |
         |User-defined configurations are added to the left. The leftmost rule matching
         |a warning message defines the action.
         |
         |Examples:
         |  - change every warning into an error: -Wconf:any:error
         |  - silence deprecations: -Wconf:cat=deprecation:s
         |  - silence a deprecation: -Wconf:origin=java\.lang\.Thread\.getId:s
         |  - silence warnings in src_managed directory: -Wconf:src=src_managed/.*:s
         |
         |Note: on the command-line you might need to quote configurations containing `*` or `&`
         |to prevent the shell from expanding patterns.""".stripMargin,
  )

  val Wshadow: Setting[List[ChoiceWithHelp[String]]] = MultiChoiceHelpSetting(
    WarningSetting,
    name = "Wshadow",
    helpArg = "warning",
    descr = "Enable or disable specific `shadow` warnings",
    choices = List(
      ChoiceWithHelp("all", ""),
      ChoiceWithHelp("private-shadow", "Warn if a private field or class parameter shadows a superclass field"),
      ChoiceWithHelp("type-parameter-shadow", "Warn when a type parameter shadows a type already in the scope"),
    ),
    default = Nil
  )

  object WshadowHas:
    // Is any choice set for -Wshadow?
    def any(using Context): Boolean = Wall.value || Wshadow.value.nonEmpty

    def allOr(s: String)(using Context): Boolean =
      Wall.value || Wshadow.value.pipe(us => us.contains("all") || us.contains(s))
    def privateShadow(using Context) =
      allOr("private-shadow")
    def typeParameterShadow(using Context) =
      allOr("type-parameter-shadow")
  end WshadowHas

  val WsafeInit: Setting[Boolean] = BooleanSetting(WarningSetting, "Wsafe-init", "Ensure safe initialization of objects.")

  object Whas:
    def allOr(s: Setting[Boolean])(using Context): Boolean =
      Wall.value || s.value
    def valueDiscard(using Context): Boolean = allOr(WvalueDiscard)
    def nonUnitStatement(using Context): Boolean = allOr(WNonUnitStatement)
    def enumCommentDiscard(using Context): Boolean = allOr(WenumCommentDiscard)
    def implausiblePatterns(using Context): Boolean = allOr(WimplausiblePatterns)
    def unstableInlineAccessors(using Context): Boolean = allOr(WunstableInlineAccessors)
    def toStringInterpolated(using Context): Boolean = allOr(WtoStringInterpolated)
    def recurseWithDefault(using Context): Boolean = allOr(WrecurseWithDefault)
    def wrongArrow(using Context): Boolean = allOr(WwrongArrow)
    def inferUnion(using Context): Boolean = allOr(WinferUnion)
    def safeInit(using Context): Boolean = allOr(WsafeInit)

/** -X "Extended" or "Advanced" settings */
private sealed trait XSettings:
  self: SettingGroup =>

  val Xhelp: Setting[Boolean] = BooleanSetting(AdvancedSetting, "X", "Print a synopsis of advanced options.")
  val XnoForwarders: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xno-forwarders", "Do not generate static forwarders in mirror classes.")
  val XmaxInlines: Setting[Int] = IntSetting(AdvancedSetting, "Xmax-inlines", "Maximal number of successive inlines.", 32)
  val XmaxInlinedTrees: Setting[Int] = IntSetting(AdvancedSetting, "Xmax-inlined-trees", "Maximal number of inlined trees.", 2_000_000)
  val Xmigration: Setting[ScalaVersion] = VersionSetting(AdvancedSetting, "Xmigration", "Warn about constructs whose behavior may have changed since version.", legacyArgs = true)
  val XprintTypes: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xprint-types", "Print tree types (debugging option).")
  val XprintDiff: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xprint-diff", "Print changed parts of the tree since last print.")
  val XprintDiffDel: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xprint-diff-del", "Print changed parts of the tree since last print including deleted parts.")
  val XprintInline: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xprint-inline", "Show where inlined code comes from.")
  val XprintSuspension: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xprint-suspension", "Show when code is suspended until macros are compiled.")
  val Xprompt: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xprompt", "Display a prompt after each error (debugging option).")
  val XreplDisableDisplay: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xrepl-disable-display", "Do not display definitions in REPL.")
  val XreplInterruptInstrumentation: Setting[String] = StringSetting(
    AdvancedSetting,
    "Xrepl-interrupt-instrumentation",
    "true|false|local",
    "pass `false` to disable bytecode instrumentation for interrupt handling in REPL, or `local` to limit interrupt support to only REPL-defined classes",
    "true"
  )
  val XverifySignatures: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xverify-signatures", "Verify generic signatures in generated bytecode.")
  val XignoreScala2Macros: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xignore-scala2-macros", "Ignore errors when compiling code that calls Scala2 macros, these will fail at runtime.")
  val XimportSuggestionTimeout: Setting[Int] = IntSetting(AdvancedSetting, "Ximport-suggestion-timeout", "Timeout (in ms) for searching for import suggestions when errors are reported.", 8000)
  val Xsemanticdb: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xsemanticdb", "Store information in SemanticDB.", aliases = List("-Ysemanticdb"))
  val XuncheckedJavaOutputVersion: Setting[String] = ChoiceSetting(AdvancedSetting, "Xunchecked-java-output-version", "target", "Emit bytecode for the specified version of the Java platform. This might produce bytecode that will break at runtime. Corresponds to -target flag in javac. When on JDK 9+, consider -java-output-version as a safer alternative.", ScalaSettingsProperties.supportedTargetVersions, "", aliases = List("-Xtarget", "--Xtarget"))
  val XcheckMacros: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xcheck-macros", "Check some invariants of macro generated code while expanding macros", aliases = List("--Xcheck-macros"))
  val XmainClass: Setting[String] = StringSetting(AdvancedSetting, "Xmain-class", "path", "Class for manifest's Main-Class entry (only useful with -d <jar>)", "")
  val XimplicitSearchLimit: Setting[Int] = IntSetting(AdvancedSetting, "Ximplicit-search-limit", "Maximal number of expressions to be generated in an implicit search", 50000)

  val XtermConflict: Setting[String] = ChoiceSetting(AdvancedSetting, "Xresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error")
  val XnoGenericSig: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val Xdumpclasses: Setting[String] = StringSetting(AdvancedSetting, "Xdump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val XjarCompressionLevel: Setting[Int] = IntChoiceSetting(AdvancedSetting, "Xjar-compression-level", "compression level to use when writing jar files", Deflater.DEFAULT_COMPRESSION to Deflater.BEST_COMPRESSION, Deflater.DEFAULT_COMPRESSION)
  val XkindProjector: Setting[String] = ChoiceSetting(AdvancedSetting, "Xkind-projector", "[underscores, enable, disable]", "Allow `*` as type lambda placeholder to be compatible with kind projector. When invoked as -Xkind-projector:underscores will repurpose `_` to be a type parameter placeholder, this will disable usage of underscore as a wildcard.", List("disable", "", "underscores"), "disable", legacyArgs = true)

  /** Documentation related settings */
  val XdropComments: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xdrop-docs", "Drop documentation when scanning source files.", aliases = List("-Xdrop-comments"))
  val XcookComments: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xcook-docs", "Cook the documentation (type check `@usecase`, etc.)", aliases = List("-Xcook-comments"))
  val XreadComments: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xread-docs", "Read documentation from tasty.")

  /** Area-specific debug output */
  val XnoEnrichErrorMessages: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xno-enrich-error-messages", "Show raw error messages, instead of enriching them with contextual information.", aliases = List("Xno-decode-stacktraces"))
  val XdebugMacros: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xdebug-macros", "Show debug info when quote pattern match fails")

  /** Pipeline compilation options */
  val XjavaTasty: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xjava-tasty", "Pickler phase should compute TASTy for .java defined symbols for use by build tools", aliases = List("-Xpickle-java", "-Yjava-tasty", "-Ypickle-java"), preferPrevious = true)
  val XearlyTastyOutput: Setting[AbstractFile] = OutputSetting(AdvancedSetting, "Xearly-tasty-output", "directory|jar", "Destination to write generated .tasty files to for use in pipelined compilation.", NoAbstractFile, aliases = List("-Xpickle-write", "-Yearly-tasty-output", "-Ypickle-write"), preferPrevious = true)
  val XallowOutlineFromTasty: Setting[Boolean] = BooleanSetting(AdvancedSetting, "Xallow-outline-from-tasty", "Allow outline TASTy to be loaded with the -from-tasty option.", aliases = List("-Yallow-outline-from-tasty"))

  val XmixinForceForwarders = ChoiceSetting(
    AdvancedSetting,
    name    = "Xmixin-force-forwarders",
    helpArg = "mode",
    descr   = "Generate forwarder methods in classes inheriting concrete methods from traits.",
    choices = List("true", "junit", "false"),
    default = "true")

  object mixinForwarderChoices {
    def isTruthy(using Context) = XmixinForceForwarders.value == "true"
    def isAtLeastJunit(using Context) = isTruthy || XmixinForceForwarders.value == "junit"
  }

  val XmacroSettings: Setting[List[String]] = MultiStringSetting(AdvancedSetting, "Xmacro-settings", "setting1,setting2,..settingN", "List of settings which exposed to the macros")

  @deprecated(message = "Superseded by -Wshadow, Scheduled for removal", since = "3.5.0")
  val Xlint: Setting[?] = BooleanSetting(AdvancedSetting, "Xlint", "Enable or disable specific warnings", deprecation = Some(Deprecation("Use -Wshadow to enable shadowing lints. Scheduled for removal.")), ignoreInvalidArgs = true)

end XSettings

/** -Y "Forking" as in forked tongue or "Private" settings */
private sealed trait YSettings:
  self: SettingGroup =>

  val Yhelp: Setting[Boolean] = BooleanSetting(ForkSetting, "Y", "Print a synopsis of private options.")
  val Ycheck: Setting[List[String]] = PhasesSetting(ForkSetting, "Ycheck", "Check the tree at the end of")
  val YcheckMods: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycheck-mods", "Check that symbols and their defining trees have modifiers in sync.")
  val Ydebug: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug", "Increase the quantity of debugging output.")
  val YdebugTrace: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-trace", "Trace core operations.")
  val YdebugFlags: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-flags", "Print all flags of definitions.")
  val YdebugMissingRefs: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-missing-refs", "Print a stacktrace when a required symbol is missing.")
  val YdebugNames: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-names", "Show internal representation of names.")
  val YdebugPos: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-pos", "Show full source positions including spans.")
  val YdebugTreeWithId: Setting[Int] = IntSetting(ForkSetting, "Ydebug-tree-with-id", "Print the stack trace when the tree with the given id is created.", Int.MinValue)
  val YdebugTypeError: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-type-error", "Print the stack trace when a TypeError is caught", false)
  val YdebugError: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-error", "Print the stack trace when any error is caught.", false)
  val YdebugUnpickling: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-unpickling", "Print the stack trace when an error occurs when reading Tasty.", false)
  val YdebugCyclic: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-cyclic", "Print the stack trace when a cyclic reference error occurs.", false)
  val Ylog: Setting[List[String]] = PhasesSetting(ForkSetting, "Ylog", "Log operations during")
  val YlogClasspath: Setting[Boolean] = BooleanSetting(ForkSetting, "Ylog-classpath", "Output information about what classpath is being applied.")
  val YdisableFlatCpCaching: Setting[Boolean] = BooleanSetting(ForkSetting, "YdisableFlatCpCaching", "Do not cache flat classpath representation of classpath elements from jars across compiler instances.")

  val Yreporter: Setting[String] = StringSetting(ForkSetting, name = "Yreporter", helpArg = "<class>", descr = "Specify a dotty.tools.dotc.reporting.Reporter", default = "dotty.tools.dotc.reporting.ConsoleReporter")

  val Yscala2Unpickler: Setting[String] = StringSetting(ForkSetting, "Yscala2-unpickler", "", "Control where we may get Scala 2 symbols from. This is either \"always\", \"never\", or a classpath.", "always")

  val YnoImports: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val Yimports: Setting[List[String]] = MultiStringSetting(ForkSetting, "Yimports", helpArg="", "Custom root imports. If set, none of scala.*, java.lang.*, or Predef.* will be imported unless explicitly included.")
  val YnoPredef: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-predef", "Compile without importing Predef.")
  val Yskip: Setting[List[String]] = PhasesSetting(ForkSetting, "Yskip", "Skip")
  val YbackendParallelism: Setting[Int] = IntChoiceSetting(ForkSetting, "Ybackend-parallelism", "maximum worker threads for backend", 1 to 16, 1)
  val YbackendWorkerQueue: Setting[Int] = IntChoiceSetting(ForkSetting, "Ybackend-worker-queue", "backend threads worker queue size", 0 to 1000, 0)
  val YstopAfter: Setting[List[String]] = PhasesSetting(ForkSetting, "Ystop-after", "Stop after", aliases = List("-stop")) // backward compat
  val YstopBefore: Setting[List[String]] = PhasesSetting(ForkSetting, "Ystop-before", "Stop before") // stop before erasure as long as we have not debugged it fully
  val YshowSuppressedErrors: Setting[Boolean] = BooleanSetting(ForkSetting, "Yshow-suppressed-errors", "Also show follow-on errors and warnings that are normally suppressed.")
  val YdetailedStats: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydetailed-stats", "Show detailed internal compiler stats (needs Stats.enabled to be set to true).")
  val YprintPos: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-pos", "Show tree positions.")
  val YprintPosSyms: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-pos-syms", "Show symbol definitions positions.")
  val YnoDeepSubtypes: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-deep-subtypes", "Throw an exception on deep subtyping call stacks.")
  val YnoSuspendedUnits: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-suspended-units", "Do not suspend units, e.g. when calling a macro defined in the same run. This will error instead of suspending.")
  val YnoPatmatOpt: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-patmat-opt", "Disable all pattern matching optimizations.")
  val YplainPrinter: Setting[Boolean] = BooleanSetting(ForkSetting, "Yplain-printer", "Pretty-print using a plain printer.")
  val YprintSyms: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-syms", "When printing trees print info in symbols instead of corresponding info in trees.")
  val YprintDebug: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-debug", "When printing trees, print some extra information useful for debugging.")
  val YprintDebugOwners: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-debug-owners", "When printing trees, print owners of definitions.")
  val YprintLevel: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-level", "print nesting levels of symbols and type variables.")
  val YshowPrintErrors: Setting[Boolean] = BooleanSetting(ForkSetting, "Yshow-print-errors", "Don't suppress exceptions thrown during tree printing.")
  val YprintTasty: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprint-tasty", "Prints the generated TASTY to stdout.")
  val YtestPickler: Setting[Boolean] = BooleanSetting(ForkSetting, "Ytest-pickler", "Self-test for pickling functionality; should be used with -Ystop-after:pickler.")
  val YtestPicklerCheck: Setting[Boolean] = BooleanSetting(ForkSetting, "Ytest-pickler-check", "Self-test for pickling -print-tasty output; should be used with -Ytest-pickler.")
  val YcheckReentrant: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycheck-reentrant", "Check that compiled program does not contain vars that can be accessed from a global root.")
  val YforceSbtPhases: Setting[Boolean] = BooleanSetting(ForkSetting, "Yforce-sbt-phases", "Run the phases used by sbt for incremental compilation (ExtractDependencies and ExtractAPI) even if the compiler is ran outside of sbt, for debugging.")
  val YdumpSbtInc: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydump-sbt-inc", "For every compiled foo.scala, output the API representation and dependencies used for sbt incremental compilation in foo.inc, implies -Yforce-sbt-phases.")
  val YcheckAllPatmat: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycheck-all-patmat", "Check exhaustivity and redundancy of all pattern matching (used for testing the algorithm).")
  val YcheckConstraintDeps: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycheck-constraint-deps", "Check dependency tracking in constraints (used for testing the algorithm).")
  val YretainTrees: Setting[Boolean] = BooleanSetting(ForkSetting, "Yretain-trees", "Retain trees for top-level classes, accessible from ClassSymbol#tree")
  val YshowTreeIds: Setting[Boolean] = BooleanSetting(ForkSetting, "Yshow-tree-ids", "Uniquely tag all tree nodes in debugging output.")
  val YfromTastyIgnoreList: Setting[List[String]] = MultiStringSetting(ForkSetting, "Yfrom-tasty-ignore-list", "file", "List of `tasty` files in jar files that will not be loaded when using -from-tasty.")
  val YlegacyLazyVals: Setting[Boolean] = BooleanSetting(ForkSetting, "Ylegacy-lazy-vals", "Use legacy (pre 3.3.0) implementation of lazy vals.")
  val YcompileScala2Library: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycompile-scala2-library", "Used when compiling the Scala 2 standard library.")
  val YprofileEnabled: Setting[Boolean] = BooleanSetting(ForkSetting, "Yprofile-enabled", "Enable profiling.")
  val YprofileDestination: Setting[String] = StringSetting(ForkSetting, "Yprofile-destination", "file", "Where to send profiling output - specify a file, default is to the console.", "", depends = List(YprofileEnabled -> true))
  val YprofileExternalTool: Setting[List[String]] = PhasesSetting(ForkSetting, "Yprofile-external-tool", "Enable profiling for a phase using an external tool hook. Generally only useful for a single phase.", "typer", depends = List(YprofileEnabled -> true))
  val YprofileRunGcBetweenPhases: Setting[List[String]] = PhasesSetting(ForkSetting, "Yprofile-run-gc", "Run a GC between phases - this allows heap size to be accurate at the expense of more time. Specify a list of phases, or *", "_", depends = List(YprofileEnabled -> true))
  val YprofileTrace: Setting[String]  = StringSetting(ForkSetting, "Yprofile-trace", "file", s"Capture trace of compilation in JSON Chrome Trace format to the specified file. This option requires ${YprofileEnabled.name}. The output file can be visualized using https://ui.perfetto.dev/.", "", depends = List(YprofileEnabled -> true))

  val YbestEffort: Setting[Boolean] = BooleanSetting(ForkSetting, "Ybest-effort", "Enable best-effort compilation attempting to produce betasty to the META-INF/best-effort directory, regardless of errors, as part of the pickler phase.")
  val YwithBestEffortTasty: Setting[Boolean] = BooleanSetting(ForkSetting, "Ywith-best-effort-tasty", "Allow to compile using best-effort tasty files. If such file is used, the compiler will stop after the pickler phase.")

  val YmagicOffsetHeader: Setting[String] = StringSetting(ForkSetting, "Ymagic-offset-header", "header", "Specify the magic header comment that marks the start of the actual code in generated wrapper scripts. Example: -Ymagic-offset-header:SOURCE_CODE_START. Then, in the source, the magic comment `///SOURCE_CODE_START:<ORIGINAL_FILE_PATH>` marks the start of user code. The comment should be suffixed by `:<ORIGINAL_FILE_PATH>` to indicate the original file.", "")

  // Experimental language features
  @deprecated(message = "This flag has no effect and will be removed in a future version.", since = "3.7.0")
  val YnoKindPolymorphism: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-kind-polymorphism", "Disable kind polymorphism. (This flag has no effect)", deprecation = Deprecation.removed())
  val YexplicitNulls: Setting[Boolean] = BooleanSetting(ForkSetting, "Yexplicit-nulls", "Make reference types non-nullable. Nullable types can be expressed with unions: e.g. String|Null.")
  val YnoFlexibleTypes: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-flexible-types", "Disable turning nullable Java return types and parameter types into flexible types, which behave like abstract types with a nullable lower bound and non-nullable upper bound.")
  val YflexifyTasty: Setting[Boolean] = BooleanSetting(ForkSetting, "Yflexify-tasty", "Apply flexification to Scala code compiled without -Yexplicit-nulls, when reading from tasty.")
  val YsafeInitGlobal: Setting[Boolean] = BooleanSetting(ForkSetting, "Ysafe-init-global", "Check safe initialization of global objects.")
  val YrequireTargetName: Setting[Boolean] = BooleanSetting(ForkSetting, "Yrequire-targetName", "Warn if an operator is defined without a @targetName annotation.")
  val YrecheckTest: Setting[Boolean] = BooleanSetting(ForkSetting, "Yrecheck-test", "Run basic rechecking (internal test only).")
  val YccDebug: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycc-debug", "Used in conjunction with captureChecking language import, debug info for captured references.")
  val YccNew: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycc-new", "Used in conjunction with captureChecking language import, try out new variants (debug option)")
  val YccLog: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycc-log", "Used in conjunction with captureChecking language import, print tracing and debug info")
  val YccVerbose: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycc-verbose", "Print root capabilities with more details")
  val YccPrintSetup: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycc-print-setup", "Used in conjunction with captureChecking language import, print trees after cc.Setup phase")

  /** Area-specific debug output */
  val YexplainLowlevel: Setting[Boolean] = BooleanSetting(ForkSetting, "Yexplain-lowlevel", "When explaining type errors, show types at a lower level.")
  val YnoDoubleBindings: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-double-bindings", "Assert no namedtype is bound twice (should be enabled only if program is error-free).")
  val YshowVarBounds: Setting[Boolean] = BooleanSetting(ForkSetting, "Yshow-var-bounds", "Print type variables with their bounds.")

  val Yinstrument: Setting[Boolean] = BooleanSetting(ForkSetting, "Yinstrument", "Add instrumentation code that counts allocations and closure creations.")
  val YinstrumentDefs: Setting[Boolean] = BooleanSetting(ForkSetting, "Yinstrument-defs", "Add instrumentation code that counts method calls; needs -Yinstrument to be set, too.")
  val YimplicitToGiven: Setting[Boolean] = BooleanSetting(ForkSetting, "Yimplicit-to-given", "Allows to rewrite the implicit keywords to their scala-3 given counterparts. Does not adjust imports. Use in conjunction with --rewrite.")

  // Deprecated: lifted from -Y to -X
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YtermConflict: Setting[String] = ChoiceSetting(ForkSetting, "Yresolve-term-conflict", "strategy", "Resolve term conflicts", List("package", "object", "error"), "error", deprecation = Deprecation.renamed("-Xresolve-term-conflict"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YnoGenericSig: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-generic-signatures", "Suppress generation of generic signatures for Java.", deprecation = Deprecation.renamed("-Xno-generic-signatures"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val Ydumpclasses: Setting[String] = StringSetting(ForkSetting, "Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", default = "", deprecation = Deprecation.renamed("-Xdump-classes"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YjarCompressionLevel: Setting[Int] = IntChoiceSetting(ForkSetting, "Yjar-compression-level", "compression level to use when writing jar files", Deflater.DEFAULT_COMPRESSION to Deflater.BEST_COMPRESSION, Deflater.DEFAULT_COMPRESSION, deprecation = Deprecation.renamed("-Xjar-compression-level"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YkindProjector: Setting[String] = ChoiceSetting(ForkSetting, "Ykind-projector", "[underscores, enable, disable]", "Allow `*` as type lambda placeholder to be compatible with kind projector. When invoked as -Ykind-projector:underscores will repurpose `_` to be a type parameter placeholder, this will disable usage of underscore as a wildcard.", List("disable", "", "underscores"), "disable", legacyArgs = true, deprecation = Deprecation.renamed("-Xkind-projector"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YdropComments: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydrop-docs", "Drop documentation when scanning source files.", aliases = List("-Ydrop-comments"), deprecation = Deprecation.renamed("-Xdrop-docs"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YcookComments: Setting[Boolean] = BooleanSetting(ForkSetting, "Ycook-docs", "Cook the documentation (type check `@usecase`, etc.)", aliases = List("-Ycook-comments"), deprecation = Deprecation.renamed("-Xcook-docs"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YreadComments: Setting[Boolean] = BooleanSetting(ForkSetting, "Yread-docs", "Read documentation from tasty.", deprecation = Deprecation.renamed("-Xread-docs"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YnoDecodeStacktraces: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-decode-stacktraces", "Show raw StackOverflow stacktraces, instead of decoding them into triggering operations.", deprecation = Deprecation.renamed("-Xno-enrich-error-messages"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YnoEnrichErrorMessages: Setting[Boolean] = BooleanSetting(ForkSetting, "Yno-enrich-error-messages", "Show raw error messages, instead of enriching them with contextual information.", deprecation = Deprecation.renamed("-Xno-enrich-error-messages"))
  @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.5.0")
  val YdebugMacros: Setting[Boolean] = BooleanSetting(ForkSetting, "Ydebug-macros", "Show debug info when quote pattern match fails", deprecation = Deprecation.renamed("-Xdebug-macros"))

  // @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.7.0")
  // val YjavaTasty: Setting[Boolean] = BooleanSetting(ForkSetting, "Yjava-tasty", "Pickler phase should compute TASTy for .java defined symbols for use by build tools", aliases = List("-Ypickle-java"), preferPrevious = true, deprecation = Deprecation.lifted("-Xjava-tasty"))
  // @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.7.0")
  // val YearlyTastyOutput: Setting[AbstractFile] = OutputSetting(ForkSetting, "Yearly-tasty-output", "directory|jar", "Destination to write generated .tasty files to for use in pipelined compilation.", NoAbstractFile, aliases = List("-Ypickle-write"), preferPrevious = true, deprecation = Deprecation.lifted("-Xearly-tasty-output"))
  // @deprecated(message = "Lifted to -X, Scheduled for removal.", since = "3.7.0")
  // val YallowOutlineFromTasty: Setting[Boolean] = BooleanSetting(ForkSetting, "Yallow-outline-from-tasty", "Allow outline TASTy to be loaded with the -from-tasty option.", deprecation = Deprecation.lifted("-Xallow-outline-from-tasty"))

  // Deprecated: lifted from -Y to -W
  @deprecated(message = "Lifted to -W, Scheduled for removal.", since = "3.5.0")
  val YcheckInit: Setting[Boolean] = BooleanSetting(ForkSetting, "Ysafe-init", "Ensure safe initialization of objects.", deprecation = Deprecation.renamed("-Wsafe-init"))

  // Deprecated: Scheduled for removal
  @deprecated(message = "Scheduled for removal.", since = "3.5.0")
  val YoutputOnlyTasty: Setting[Boolean] = BooleanSetting(ForkSetting, "Youtput-only-tasty", "Used to only generate the TASTy file without the classfiles", deprecation = Deprecation.removed())
end YSettings

