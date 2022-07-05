package dotty.tools.scaladoc

import java.util.ServiceLoader
import java.io.File
import java.io.FileWriter
import java.util.jar._
import scala.jdk.CollectionConverters._
import collection.immutable.ArraySeq

import java.nio.file.{ Files, Paths }

import dotty.tools.dotc.config.Settings._
import dotty.tools.dotc.config.{ CommonScalaSettings, AllScalaSettings }
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.core.Contexts._

import dotty.tools.scaladoc.Inkuire
import dotty.tools.scaladoc.Inkuire._

object Scaladoc:
  case class Args(
    name: String,
    tastyDirs: Seq[File] = Nil,
    tastyFiles: Seq[File] = Nil,
    classpath: String = "",
    bootclasspath: String = "",
    output: File,
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
    versionsDictionaryUrl: Option[String] = None,
    generateInkuire : Boolean = false,
    apiSubdirectory : Boolean = false,
    scastieConfiguration: String = "",
    defaultTemplate: Option[String] = None,
    quickLinks: List[QuickLink] = List.empty
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

        if (parsedArgs.output.exists()) util.IO.delete(parsedArgs.output)

        run(updatedArgs)
        report.inform("Done")
      else report.error("Failure")

      if parsedArgs.generateInkuire then dumpInkuireDB(parsedArgs.output.getAbsolutePath, parsedArgs)
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
    val newContext = rootCtx.fresh
    given CompilerContext = newContext
    val ss = ScaladocSettings()
    import ss._
    val summary = ScaladocCommand.distill(args, ss)()
    val argumentFilesOrNone = ScaladocCommand.checkUsage(summary, true)(using ss)(using summary.sstate)

    extension[T](arg: Setting[T])
      def get = arg.valueIn(summary.sstate)
      def withDefault(default: => T) =
        if arg.get == arg.default then default else arg.get
      def nonDefault =
        if arg.get == arg.default then None else Some(arg.get)

    def setInGlobal[T](s: Setting[T]) =
      s.nonDefault.foreach { newValue =>
        newContext.settings.allSettings.find(_ == s).fold(
          report.warning(s"Unable to set ${s.name} in global context")
        )(s => newContext.setSetting(s.asInstanceOf[Setting[T]], newValue))
      }

    val commonScalaSettings = (new SettingGroup with CommonScalaSettings).allSettings
    val allScalaSettings = (new SettingGroup with AllScalaSettings).allSettings

    val (shared, other) = allSettings
      .filter(s => !s.isDefaultIn(summary.sstate))
      .filter(allScalaSettings.contains)
      .partition(commonScalaSettings.contains)
    shared.foreach(setInGlobal)

    if !other.isEmpty then report.echo(s"Skipping unused scalacOptions: ${other.map(_.name).mkString(", ")}")

    def parseTastyRoots(roots: String) =
      roots.split(File.pathSeparatorChar).toList.map(new File(_))

    argumentFilesOrNone.fold((None, newContext)) { argumentFiles =>
      val inFiles = argumentFiles.map(File(_)).filter(_.getName != "___fake___.scala")
      val (existing, nonExisting) = inFiles.partition(_.exists)

      if nonExisting.nonEmpty then report.warning(
        s"scaladoc will ignore following non-existent paths: ${nonExisting.mkString(", ")}"
      )

      val (dirs, files) = existing.partition(_.isDirectory)
      val (validFiles, other) = files.partition(f =>
        f.getName.endsWith(".tasty") || f.getName.endsWith(".jar")
      )

      if other.nonEmpty then report.warning(
        s"scaladoc suports only .tasty and .jar files, following files will be ignored: ${other.mkString(", ")}"
      )

      def defaultDest(): File =
        report.warning("Destination is not provided, please provide '-d' parameter pointing to directory where docs should be created")
        File("output")

      val legacySourceLinkList = if legacySourceLink.get.nonEmpty then List(legacySourceLink.get) else Nil

      val externalMappings =
        externalDocumentationMappings.get.flatMap( s =>
            ExternalDocLink.parse(s).fold(left => {
              report.warning(left)
              None
            }, right => Some(right)
          )
        )

      val legacyExternalMappings =
        legacyExternalDocumentationMappings.get.flatMap { s =>
          ExternalDocLink.parseLegacy(s).fold(left => {
              report.warning(left)
              None
            }, right => Some(right)
          )
        }

      val socialLinksParsed =
        socialLinks.get.flatMap { s =>
          SocialLinks.parse(s).fold(left => {
            report.warning(left)
            None
          },right => Some(right))
        }

      val quickLinksParsed =
        quickLinks.get.flatMap { s =>
          QuickLink.parse(s) match
            case Left(err) =>
              report.warning(err)
              None
            case Right(value) => Some(value)
        }

      unsupportedSettings.filter(s => s.get != s.default).foreach { s =>
        report.warning(s"Setting ${s.name} is currently not supported.")
      }
      val destFile = outputDir.nonDefault.fold(defaultDest())(_.file)
      val printableProjectName = projectName.nonDefault.fold("")("for " + _ )
      report.inform(
        s"Generating documentation $printableProjectName in $destFile")

      if deprecatedSkipPackages.get.nonEmpty then report.warning(deprecatedSkipPackages.description)

      val docArgs = Args(
        projectName.withDefault("root"),
        dirs,
        validFiles,
        classpath.get,
        bootclasspath.get,
        destFile,
        siteRoot.nonDefault,
        projectVersion.nonDefault,
        projectLogo.nonDefault,
        projectFooter.nonDefault,
        syntax.get,
        sourceLinks.get ++ legacySourceLinkList,
        revision.nonDefault,
        externalMappings ++ legacyExternalMappings,
        socialLinksParsed,
        skipById.get ++ deprecatedSkipPackages.get,
        skipByRegex.get,
        docRootContent.nonDefault,
        author.get,
        groups.get,
        visibilityPrivate.get,
        docCanonicalBaseUrl.get,
        YdocumentSyntheticTypes.get,
        snippetCompiler.get,
        noLinkWarnings.get,
        versionsDictionaryUrl.nonDefault,
        generateInkuire.get,
        apiSubdirectory.get,
        scastieConfiguration.get,
        defaultTemplate.nonDefault,
        quickLinksParsed
      )
      (Some(docArgs), newContext)
    }

  private [scaladoc] def run(args: Args)(using ctx: CompilerContext): DocContext =
    given docContext: DocContext = new DocContext(args, ctx)
    val module = ScalaModuleProvider.mkModule()
    new dotty.tools.scaladoc.renderers.HtmlRenderer(module.rootPackage, module.members).render()
    docContext.reportPathCompatIssues()
    report.inform("generation completed successfully")
    docContext
