package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.DokkaSourceSetImpl
import org.jetbrains.dokka.plugability.DokkaContext
import java.io.File
import collection.JavaConverters._
import dotty.dokka.site.StaticSiteContext
import dotty.tools.dotc.core.Contexts._
import java.io.ByteArrayOutputStream
import java.io.PrintStream

type CompilerContext = dotty.tools.dotc.core.Contexts.Context

given compilerContext(using docContext: DocContext) as CompilerContext =
  docContext.compilerContext

given docContextFromDokka(using dokkaContext: DokkaContext) as DocContext =
  dokkaContext.getConfiguration.asInstanceOf[DocContext]

val report = dotty.tools.dotc.report

def throwableToString(t: Throwable)(using CompilerContext): String =
  if ctx.settings.verbose.value then
    val os = new ByteArrayOutputStream
    t.printStackTrace(new PrintStream(os))
    os.toString()
  else s"${t.getClass.getName}: ${t.getMessage}"

// TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
private def createMessage(
  msg: String, file: File, e: Throwable | Null)(using CompilerContext): String =
    val localizedMessage = s"$file: $msg"
    e match
      case null => localizedMessage
      case throwable: Throwable =>
         s"$localizedMessage \ncaused by: ${throwableToString(throwable)}"

extension (r: report.type):
  def error(m: String, f: File, e: Throwable | Null = null)(using CompilerContext): Unit =
    r.error(createMessage(m, f, e))

  def warn(m: String, f: File, e: Throwable)(using CompilerContext): Unit =
    r.warning(createMessage(m, f, e))

  def warn(m: String, f: File)(using CompilerContext): Unit =
    r.warning(createMessage(m, f, null))


case class DocContext(args: Scala3doc.Args, compilerContext: CompilerContext)
  extends DokkaConfiguration:
    override def getOutputDir: File = args.output
    override def getCacheRoot: File = null
    override def getOfflineMode: Boolean = false
    override def getFailOnWarning: Boolean = false
    override def getSourceSets: JList[DokkaSourceSet] = JList(mkSourceSet)
    override def getModules: JList[DokkaConfiguration.DokkaModuleDescription] = JList()
    override def getPluginsClasspath: JList[File] = JList()
    override def getModuleName(): String = "ModuleName"
    override def getModuleVersion(): String = ""

    lazy val sourceLinks: SourceLinks = SourceLinks.load(using this)

    lazy val displaySourceSets = getSourceSets.toDisplaySourceSet

    val logger = new Scala3DocDokkaLogger(using compilerContext)

    lazy val staticSiteContext = args.docsRoot.map(path => StaticSiteContext(
        File(path).getAbsoluteFile(),
        Set(mkSourceSet.asInstanceOf[SourceSetWrapper]),
        args,
        sourceLinks
      )(using compilerContext))

    override def getPluginsConfiguration: JList[DokkaConfiguration.PluginConfiguration] =
      JList()

    val mkSourceSet: DokkaSourceSet =
      new DokkaSourceSetImpl(
        /*displayName=*/ args.name,
        /*sourceSetID=*/ new DokkaSourceSetID(args.name, "main"),
        /*classpath=*/ JList(),
        /*sourceRoots=*/ JSet(),
        /*dependentSourceSets=*/ JSet(),
        /*samples=*/ JSet(),
        /*includes=*/ JSet(),
        /*includeNonPublic=*/ true,
        /* changed because of exception in reportUndocumentedTransformer - there's 'when' which doesnt match because it contains only KotlinVisbility cases */
        /*reportUndocumented=*/ false,
        // Now all our packages are empty from dokka perspective
        /*skipEmptyPackages=*/ false,
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

    val sourceSet = mkSourceSet.asInstanceOf[SourceSetWrapper]
