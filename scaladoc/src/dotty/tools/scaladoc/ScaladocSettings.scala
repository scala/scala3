package dotty.tools.scaladoc

import java.util.ServiceLoader
import java.io.File
import java.util.jar._
import collection.JavaConverters._
import collection.immutable.ArraySeq

import java.nio.file.Files

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.AllScalaSettings
import dotty.tools.scaladoc.Scaladoc._
import dotty.tools.dotc.config.Settings.Setting.value
import dotty.tools.dotc.config.Properties._
import dotty.tools.dotc.config.CliCommand
import dotty.tools.dotc.core.Contexts._

class ScaladocSettings extends SettingGroup with AllScalaSettings:
  val unsupportedSettings = Seq(
    // Options that we like to support
    bootclasspath, extdirs, javabootclasspath, encoding, usejavacp,
    // Needed for plugin architecture
    plugin,disable,require, pluginsDir, pluginOptions,
    // we need support for sourcepath and sourceroot
    sourcepath, sourceroot
  )

  val projectName: Setting[String] =
    StringSetting("-project", "project title", "The name of the project.", "", aliases = List("-doc-title"))

  val projectVersion: Setting[String] =
    StringSetting("-project-version", "project version", "The current version of your project.", "", aliases = List("-doc-version"))

  val projectLogo: Setting[String] =
    StringSetting("-project-logo", "project logo filename", "The file that contains the project's logo (in /images).", "", aliases = List("-doc-logo"))

  val projectFooter: Setting[String] = StringSetting("-project-footer", "project footer", "A footer on every Scaladoc page.", "", aliases = List("-doc-footer"))

  val sourceLinks: Setting[List[String]] =
    MultiStringSetting("-source-links", "sources", SourceLinks.usage)

  val syntax: Setting[String] =
    StringSetting("-comment-syntax", "syntax", "Syntax of the comment used", "")

  val revision: Setting[String] =
    StringSetting("-revision", "revision", "Revision (branch or ref) used to build project project", "")

  val externalDocumentationMappings: Setting[List[String]] =
    MultiStringSetting("-external-mappings", "external-mappings",
      "Mapping between regexes matching classpath entries and external documentation. " +
        "'regex::[scaladoc|scaladoc|javadoc]::path' syntax is used")

  val socialLinks: Setting[List[String]] =
    MultiStringSetting("-social-links", "social-links",
      "Links to social sites. '[github|twitter|gitter|discord]::link' syntax is used. " +
        "'custom::link::white_icon_name::black_icon_name' is also allowed, in this case icons must be present in 'images/'' directory.")

  val deprecatedSkipPackages: Setting[List[String]] =
    MultiStringSetting("-skip-packages", "packages", "Deprecated, please use `-skip-by-id` or `-skip-by-regex`")

  val skipById: Setting[List[String]] =
    MultiStringSetting("-skip-by-id", "package or class identifier", "Identifiers of packages or top-level classes to skip when generating documentation")

  val skipByRegex: Setting[List[String]] =
    MultiStringSetting("-skip-by-regex", "regex", "Regexes that match fully qualified names of packages or top-level classes to skip when generating documentation")

  val docRootContent: Setting[String] =
    StringSetting("-doc-root-content", "path", "The file from which the root package documentation should be imported.", "")

  val author: Setting[Boolean] =
    BooleanSetting("-author", "Include authors.", false)

  val groups: Setting[Boolean] =
    BooleanSetting("-groups", "Group similar functions together (based on the @group annotation)", false)

  val visibilityPrivate: Setting[Boolean] =
    BooleanSetting("-private", "Show all types and members. Unless specified, show only public and protected types and members.", false)

  val docCanonicalBaseUrl: Setting[String] =
    StringSetting(
      "-doc-canonical-base-url",
      "url",
      s"A base URL to use as prefix and add `canonical` URLs to all pages. The canonical URL may be used by search engines to choose the URL that you want people to see in search results. If unset no canonical URLs are generated.",
      ""
    )

  val siteRoot: Setting[String] = StringSetting(
    "-siteroot",
    "site root",
    "A directory containing static files from which to generate documentation.",
    "./docs"
  )

  val YdocumentSyntheticTypes: Setting[Boolean] =
    BooleanSetting("-Ydocument-synthetic-types", "Documents intrinsic types e. g. Any, Nothing. Setting is useful only for stdlib", false)
