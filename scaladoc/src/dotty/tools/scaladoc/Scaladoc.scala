package dotty.tools.scaladoc

import java.io.File
import java.io.FileWriter
import java.nio.file.Paths

import collection.immutable.ArraySeq

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.{ CommonScalaSettings, AllScalaSettings }
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.core.Contexts._
import dotty.tools.scaladoc.Inkuire._

object Scaladoc:
  case class Args(
    name: String,
    tastyDirs: Seq[File] = Nil,
    tastyFiles: Seq[File] = Nil,
    classpath: String = "",
    bootclasspath: String = "",
    output: File | Null,
    docsRoot: Option[String] = None,
    projectVersion: Option[String] = None,
    projectLogo: Option[String] = None,
    projectFooter: Option[String] = None,
    defaultSyntax: List[String] = Nil,
    sourceLinks: List[String] = Nil,
    revision: Option[String] = None,
    externalMappings: List[ExternalDocLink] = Nil,
    socialLinks: List[SocialLinks] = Nil,
    identifiersToSkip: List[String] = Nil,
    regexesToSkip: List[String] = Nil,
    rootDocPath: Option[String] = None,
    includeAuthors: Boolean = false,
    includeGroups: Boolean = false,
    includePrivateAPI: Boolean = false,
    docCanonicalBaseUrl: String = "",
    documentSyntheticTypes: Boolean = false,
    snippetCompiler: List[String] = Nil,
    noLinkWarnings: Boolean = false,
    noLinkAssetWarnings: Boolean = false,
    versionsDictionaryUrl: Option[String] = None,
    generateInkuire : Boolean = false,
    apiSubdirectory : Boolean = false,
    scastieConfiguration: String = "",
    defaultTemplate: Option[String] = None,
    quickLinks: List[QuickLink] = List.empty,
    dynamicSideMenu: Boolean = false,
    suppressCC: Boolean = false, // suppress rendering anything related to experimental capture checking
    noSnippetNamesFor: List[String] = Nil,
    generateApi: Boolean = true, // generate API documentation
  )

  def run(args: Array[String], rootContext: CompilerContext): Reporter =
    val (parsedArgsOrNone, ctx) = extract(args, rootContext)

    parsedArgsOrNone.map { parsedArgs =>
      given CompilerContext = ctx

      def listTastyFiles(f: File): Seq[File] =
        val (files, dirs) = Option(f.listFiles()).toArray.flatten.partition(_.isFile)
        ArraySeq.unsafeWrapArray(
          files.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)
        )
      val tastyFiles = parsedArgs.tastyFiles ++ parsedArgs.tastyDirs.flatMap(listTastyFiles)

      if !ctx.reporter.hasErrors then
        val updatedArgs = parsedArgs.copy(tastyDirs = parsedArgs.tastyDirs, tastyFiles = tastyFiles)

        if (parsedArgs.output.nn.exists()) util.IO.delete(parsedArgs.output)

        run(updatedArgs)
        report.inform("Done")
      else report.error("Failure")

      if parsedArgs.generateInkuire then dumpInkuireDB(parsedArgs.output.nn.getAbsolutePath, parsedArgs)
    }

    ctx.reporter

  def dumpInkuireDB(output: String, parsedArgs: Args) = {
    val dbPath = Paths.get(output, "inkuire-db.json")
    val dbFile = dbPath.toFile()
    dbFile.createNewFile()
    val dbWriter = new FileWriter(dbFile, false)
    Inkuire.beforeSave()
    dbWriter.write(s"${EngineModelSerializers.serialize(Inkuire.db)}")
    dbWriter.close()

    val configPath = Paths.get(output, "scripts/inkuire-config.json")
    val configFile = configPath.toFile()
    configFile.createNewFile()
    val configWriter = new FileWriter(configFile, false)
    configWriter.write(Inkuire.generateInkuireConfig(parsedArgs.externalMappings.map(_.documentationUrl.toString)))
    configWriter.close()
  }

  def extract(args: Array[String], rootCtx: CompilerContext): (Option[Scaladoc.Args], CompilerContext) =
    val ictx = rootCtx.fresh
    // Unfortunately, `Context` is only meant to work with `ScalaSettings`, but we want to use `ScaladocSettings`...
    val ss = new ScaladocSettings()
    val summary = ScaladocCommand.distill(args, ss)(ss.defaultState)(using ictx)
    ictx.setSettings(summary.sstate)

    given CompilerContext = ictx
    val argumentFilesOrNone = ScaladocCommand.checkUsage(summary, true)(using ss)(using ictx.settingsState)

    def setInGlobal[T](s: Setting[T]) =
      s.valueSetByUser.foreach { newValue =>
        ss.allSettings.find(_ == s).fold(
          report.warning(s"Unable to set ${s.name} in global context")
        )(s => ictx.setSetting(s.asInstanceOf[Setting[T]], newValue))
      }

    val commonScalaSettings = (new SettingGroup with CommonScalaSettings).allSettings
    val allScalaSettings = (new SettingGroup with AllScalaSettings).allSettings

    val (shared, other) = ss.allSettings
      .filter(s => !s.isDefaultIn(summary.sstate))
      .filter(allScalaSettings.contains)
      .partition(commonScalaSettings.contains)
    shared.foreach(setInGlobal)

    if ss.warnOnUnusedOptions.value && other.nonEmpty then report.warning(s"Skipping unused scalacOptions: ${other.map(_.name).mkString(", ")}")

    def parseTastyRoots(roots: String) =
      roots.split(File.pathSeparatorChar).toList.map(new File(_))

    argumentFilesOrNone.fold((None, ictx)) { argumentFiles =>
      val (existing, nonExisting) = argumentFiles.map(File(_)).partition(_.exists)

      if nonExisting.nonEmpty then report.warning(
        s"scaladoc will ignore following non-existent paths: ${nonExisting.mkString(", ")}"
      )

      val (dirs, files) = existing.partition(_.isDirectory)
      val (validFiles, other) = files.partition(f =>
        f.getName.endsWith(".tasty") || f.getName.endsWith(".jar")
      )

      if other.nonEmpty then report.warning(
        s"scaladoc supports only .tasty and .jar files, following files will be ignored: ${other.mkString(", ")}"
      )

      def defaultDest(): File =
        report.warning("Destination is not provided, please provide '-d' parameter pointing to directory where docs should be created")
        File("output")

      val legacySourceLinkList = if ss.legacySourceLink.value.nonEmpty then List(ss.legacySourceLink.value) else Nil

      val externalMappings =
        ss.externalDocumentationMappings.value.flatMap( s =>
            ExternalDocLink.parse(s).fold(left => {
              report.warning(left)
              None
            }, right => Some(right)
          )
        )

      val legacyExternalMappings =
        ss.legacyExternalDocumentationMappings.value.flatMap { s =>
          ExternalDocLink.parseLegacy(s).fold(left => {
              report.warning(left)
              None
            }, right => Some(right)
          )
        }

      val socialLinksParsed =
        ss.socialLinks.value.flatMap { s =>
          SocialLinks.parse(s).fold(left => {
            report.warning(left)
            None
          },right => Some(right))
        }

      val quickLinksParsed =
        ss.quickLinks.value.flatMap { s =>
          QuickLink.parse(s) match
            case Left(err) =>
              report.warning(err)
              None
            case Right(value) => Some(value)
        }

      ss.unsupportedSettings.filter(s => !s.isDefault).foreach { s =>
        report.warning(s"Setting ${s.name} is currently not supported.")
      }
      val destFile = ss.outputDir.valueSetByUser.fold(defaultDest())(_.file)
      val printableProjectName = ss.projectName.valueSetByUser.fold("")("for " + _ )
      report.inform(
        s"Generating documentation $printableProjectName in $destFile")

      if ss.deprecatedSkipPackages.value.nonEmpty then report.warning(ss.deprecatedSkipPackages.description(short = false))

      val docArgs = Args(
        ss.projectName.valueSetByUser.getOrElse("root"),
        dirs,
        validFiles,
        ss.classpath.value,
        ss.bootclasspath.value,
        destFile,
        Option(ss.siteRoot.value),
        ss.projectVersion.valueSetByUser,
        ss.projectLogo.valueSetByUser,
        ss.projectFooter.valueSetByUser,
        ss.syntax.value,
        ss.sourceLinks.value ++ legacySourceLinkList,
        ss.revision.valueSetByUser,
        externalMappings ++ legacyExternalMappings,
        socialLinksParsed,
        ss.skipById.value ++ ss.deprecatedSkipPackages.value,
        ss.skipByRegex.value,
        ss.docRootContent.valueSetByUser,
        ss.author.value,
        ss.groups.value,
        ss.visibilityPrivate.value,
        ss.docCanonicalBaseUrl.value,
        ss.YdocumentSyntheticTypes.value,
        ss.snippetCompiler.value,
        ss.noLinkWarnings.value,
        ss.noLinkAssetWarnings.value,
        ss.versionsDictionaryUrl.valueSetByUser,
        ss.generateInkuire.value,
        ss.apiSubdirectory.value,
        ss.scastieConfiguration.value,
        ss.defaultTemplate.valueSetByUser,
        quickLinksParsed,
        ss.dynamicSideMenu.value,
        ss.suppressCC.value,
        ss.noSnippetNamesFor.value,
        ss.generateApi.value,
      )
      (Some(docArgs), ictx)
    }

  private [scaladoc] def run(args: Args)(using ctx: CompilerContext): DocContext =
    given docContext: DocContext = new DocContext(args, ctx)
    val module = ScalaModuleProvider.mkModule()
    new dotty.tools.scaladoc.renderers.HtmlRenderer(module.rootPackage, module.members).render()
    docContext.reportPathCompatIssues()
    report.inform("generation completed successfully")
    docContext
