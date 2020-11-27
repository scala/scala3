package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.DokkaSourceSetImpl
import java.io.File
import collection.JavaConverters._
import dotty.dokka.site.StaticSiteContext

case class DottyDokkaConfig(args: Scala3doc.Args, docContext: DocContext) extends DokkaConfiguration:
  override def getOutputDir: File = args.output
  override def getCacheRoot: File = null
  override def getOfflineMode: Boolean = false
  override def getFailOnWarning: Boolean = false
  override def getSourceSets: JList[DokkaSourceSet] = JList(mkSourceSet)
  override def getModules: JList[DokkaConfiguration.DokkaModuleDescription] = JList()
  override def getPluginsClasspath: JList[File] = JList()
  override def getModuleName(): String = "ModuleName"
  override def getModuleVersion(): String = ""

  lazy val sourceLinks: SourceLinks = SourceLinks.load(args)

  lazy val staticSiteContext = args.docsRoot.map(path => StaticSiteContext(
      File(path).getAbsoluteFile(),
      Set(mkSourceSet.asInstanceOf[SourceSetWrapper]),
      args,
      sourceLinks
    ))

  override def getPluginsConfiguration: JList[DokkaConfiguration.PluginConfiguration] = JList()

  lazy val mkSourceSet: DokkaSourceSet =
    new DokkaSourceSetImpl(
      /*displayName=*/ args.name,
      /*sourceSetID=*/ new DokkaSourceSetID(args.name, "main"),
      /*classpath=*/ JList(),
      /*sourceRoots=*/ JSet(),
      /*dependentSourceSets=*/ JSet(),
      /*samples=*/ JSet(),
      /*includes=*/ JSet(),
      /*includeNonPublic=*/ true,
      /*reportUndocumented=*/ false, /* changed because of exception in reportUndocumentedTransformer - there's 'when' which doesnt match because it contains only KotlinVisbility cases */
      /*skipEmptyPackages=*/ false, // Now all our packages are empty from dokka perspective
      /*skipDeprecated=*/ true,
      /*jdkVersion=*/ 8,
      /*sourceLinks=*/ JSet(),
      /*perPackageOptions=*/ JList(),
      /*externalDocumentationLinks=*/ JSet(),
      /*languageVersion=*/ null,
      /*apiVersion=*/ null,
      /*noStdlibLink=*/ true,
      /*noJdkLink=*/  true,
      /*suppressedFiles=*/  JSet(),
      /*suppressedFiles=*/  Platform.jvm
    ).asInstanceOf[DokkaSourceSet] // Why I do need to cast here? Kotlin magic?

