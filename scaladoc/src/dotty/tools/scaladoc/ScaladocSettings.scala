package dotty.tools.scaladoc

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.AllScalaSettings
import dotty.tools.dotc.config.ScalaSettingCategories.RootSetting

class ScaladocSettings extends SettingGroup with AllScalaSettings:
  val unsupportedSettings = Seq(
    // Needed for plugin architecture
    plugin, disable, require, pluginsDir, pluginOptions,
  )


  val projectName: Setting[String] =
    StringSetting(RootSetting, "project", "project title", "The name of the project.", "", aliases = List("-doc-title"))

  val projectVersion: Setting[String] =
    StringSetting(RootSetting, "project-version", "project version", "The current version of your project.", "", aliases = List("-doc-version"))

  val projectLogo: Setting[String] =
    StringSetting(RootSetting, "project-logo", "project logo filename", "Path to the file that contains the project's logo. Provided path can be absolute or relative to the project root directory.", "", aliases = List("-doc-logo"))

  val projectFooter: Setting[String] = StringSetting(RootSetting, "project-footer", "project footer", "A footer on every Scaladoc page.", "", aliases = List("-doc-footer"))

  val sourceLinks: Setting[List[String]] =
    MultiStringSetting(RootSetting, "source-links", "sources", SourceLinks.usage)

  val legacySourceLink: Setting[String] =
    StringSetting(RootSetting, "doc-source-url", "sources", "Legacy option from Scala 2. Use -source-links instead.", "")

  val syntax: Setting[List[String]] =
    MultiStringSetting(RootSetting, "comment-syntax", "syntax", tasty.comments.CommentSyntaxArgs.usage)

  val revision: Setting[String] =
    StringSetting(RootSetting, "revision", "revision", "Revision (branch or ref) used to build project project", "")

  val externalDocumentationMappings: Setting[List[String]] =
    MultiStringSetting(RootSetting, "external-mappings", "external-mappings",
      "Mapping between regexes matching classpath entries and external documentation. " +
        "'regex::[scaladoc|scaladoc|javadoc]::path' syntax is used")

  val legacyExternalDocumentationMappings: Setting[List[String]] =
    MultiStringSetting(RootSetting, "doc-external-doc", "legacy-external-mappings", "Legacy option from Scala 2. Mapping betweeen path and external documentation. Use -external-mappings instead.")

  val socialLinks: Setting[List[String]] =
    MultiStringSetting(RootSetting, "social-links", "social-links",
      "Links to social sites. '[github|twitter|gitter|discord]::link' or 'custom::link::light_icon_file_name[::dark_icon_file_name]' syntax is used. For custom links, the icons must be present in '_assets/images/'")

  val deprecatedSkipPackages: Setting[List[String]] =
    MultiStringSetting(RootSetting, "skip-packages", "packages", "Deprecated, please use `-skip-by-id` or `-skip-by-regex`")

  val skipById: Setting[List[String]] =
    MultiStringSetting(RootSetting, "skip-by-id", "package or class identifier", "Identifiers of packages or top-level classes to skip when generating documentation")

  val skipByRegex: Setting[List[String]] =
    MultiStringSetting(RootSetting, "skip-by-regex", "regex", "Regexes that match fully qualified names of packages or top-level classes to skip when generating documentation")

  val docRootContent: Setting[String] =
    StringSetting(RootSetting, "doc-root-content", "path", "The file from which the root package documentation should be imported.", "")

  val author: Setting[Boolean] =
    BooleanSetting(RootSetting, "author", "Include authors.", false)

  val groups: Setting[Boolean] =
    BooleanSetting(RootSetting, "groups", "Group similar functions together (based on the @group annotation)", false)

  val visibilityPrivate: Setting[Boolean] =
    BooleanSetting(RootSetting, "private", "Show all types and members. Unless specified, show only public and protected types and members.", false)

  val docCanonicalBaseUrl: Setting[String] =
    StringSetting(
      RootSetting,
      "doc-canonical-base-url",
      "url",
      s"A base URL to use as prefix and add `canonical` URLs to all pages. The canonical URL may be used by search engines to choose the URL that you want people to see in search results. If unset no canonical URLs are generated.",
      ""
    )

  val siteRoot: Setting[String] = StringSetting(
    RootSetting,
    "siteroot",
    "site root",
    "A directory containing static files from which to generate documentation.",
    "./docs"
  )

  val noLinkWarnings: Setting[Boolean] = BooleanSetting(
    RootSetting,
    "no-link-warnings",
    "Avoid warnings for ambiguous and incorrect links in members look up. Doesn't affect warnings for incorrect links of assets etc.",
    false
  )

  val noLinkAssetWarnings: Setting[Boolean] = BooleanSetting(
    RootSetting,
    "no-link-asset-warnings",
    "Avoid warnings for incorrect links of assets like images, static pages, etc.",
    false
  )

  val versionsDictionaryUrl: Setting[String] = StringSetting(
    RootSetting,
    "versions-dictionary-url",
    "versions dictionary url",
    "A URL pointing to a JSON document containing a dictionary `version label -> documentation location`. " +
      "The JSON file has single property \"versions\" that holds dictionary of labels of specific docs and URL pointing to their index.html top-level file. " +
      "Useful for libraries that maintain different versions of their documentation.",
    ""
  )

  val YdocumentSyntheticTypes: Setting[Boolean] =
    BooleanSetting(RootSetting, "Ydocument-synthetic-types", "Attach pages with documentation of the intrinsic types e. g. Any, Nothing to the docs. Setting is useful only for stdlib.", false)

  val snippetCompiler: Setting[List[String]] =
    MultiStringSetting(RootSetting, "snippet-compiler", "snippet-compiler", snippets.SnippetCompilerArgs.usage)

  val generateInkuire: Setting[Boolean] =
    BooleanSetting(RootSetting, "Ygenerate-inkuire", "Generates InkuireDB and enables Hoogle-like searches", false)

  val apiSubdirectory: Setting[Boolean] =
    BooleanSetting(RootSetting, "Yapi-subdirectory", "Put the API documentation pages inside a directory `api/`", false)

  val generateApi: Setting[Boolean] =
    BooleanSetting(RootSetting, "Ygenerate-api", "Controls whether API documentation should be generated. If disabled, only documentation pages will be generated", true)

  val scastieConfiguration: Setting[String] =
    StringSetting(RootSetting, "scastie-configuration", "Scastie configuration", "Additional configuration passed to Scastie in code snippets", "")

  val defaultTemplate: Setting[String] =
    StringSetting(
      RootSetting,
      "default-template",
      "default template used by static site",
      "The static site is generating empty files for indexes that haven't been provided explicitly in a sidebar/missing index.html in directory. " +
        "User can specify what default template should be used for such indexes. It can be useful for providing generic templates that interpolate some common settings, like title, or can have some custom html embedded.",
      ""
    )

  val quickLinks: Setting[List[String]] =
    MultiStringSetting(
      RootSetting,
      "quick-links",
      "quick-links",
      "List of quick links that is displayed in the header of documentation."
    )

  val dynamicSideMenu: Setting[Boolean] =
    BooleanSetting(RootSetting, "dynamic-side-menu", "Generate side menu via JS instead of embedding it in every html file", false)

  val suppressCC: Setting[Boolean] =
    BooleanSetting(RootSetting, "suppressCC", "Suppress rendering anything related to experimental capture checking", false)

  def scaladocSpecificSettings: Set[Setting[?]] =
    Set(sourceLinks, legacySourceLink, syntax, revision, externalDocumentationMappings, socialLinks, skipById, skipByRegex, deprecatedSkipPackages, docRootContent, snippetCompiler, generateInkuire, defaultTemplate, scastieConfiguration, quickLinks, dynamicSideMenu, suppressCC, generateApi)
