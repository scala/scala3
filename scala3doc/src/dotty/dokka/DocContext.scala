package dotty.dokka

import org.jetbrains.dokka._
import org.jetbrains.dokka.DokkaSourceSetImpl
import org.jetbrains.dokka.plugability.DokkaContext
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import collection.JavaConverters._
import dotty.dokka.site.StaticSiteContext
import dotty.tools.dotc.core.Contexts._
import dotty.tools.io.VirtualFile
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.io.Codec
import java.net.URL

type CompilerContext = dotty.tools.dotc.core.Contexts.Context

given compilerContext(using docContext: DocContext): CompilerContext =
  docContext.compilerContext

given docContextFromDokka(using dokkaContext: DokkaContext): DocContext =
  dokkaContext.getConfiguration.asInstanceOf[DocContext]

val report = dotty.tools.dotc.report

def relativePath(p: Path)(using Context): Path =
  val root = Paths.get("").toAbsolutePath()
  val absPath = p.toAbsolutePath
  if absPath.startsWith(root) then root.relativize(p.toAbsolutePath()) else p


def throwableToString(t: Throwable)(using CompilerContext): String =
  val os = new ByteArrayOutputStream
  t.printStackTrace(new PrintStream(os))
  val stLinkes = os.toString().linesIterator
  if ctx.settings.verbose.value then stLinkes.mkString("\n")
  else stLinkes.take(5).mkString("\n")

private def sourcePostionFor(f: File)(using CompilerContext) =
    val relPath = relativePath(f.toPath)
    val virtualFile = new VirtualFile(relPath.toString, relPath.toString)
    val sourceFile = new SourceFile(virtualFile, Codec.UTF8)
    SourcePosition(sourceFile, Spans.NoSpan)

// TODO (https://github.com/lampepfl/scala3doc/issues/238): provide proper error handling
private def createMessage(
  msg: String, file: File, e: Throwable | Null)(using CompilerContext): String =
    val localizedMessage = s"$file: $msg"
    e match
      case null => localizedMessage
      case throwable: Throwable =>
         s"$localizedMessage \ncaused by: ${throwableToString(throwable)}"

extension (r: report.type)
  def error(m: String, f: File, e: Throwable | Null = null)(using CompilerContext): Unit =
    r.error(createMessage(m, f, e), sourcePostionFor(f))

  def warn(m: String, f: File, e: Throwable)(using CompilerContext): Unit =
    r.warning(createMessage(m, f, e), sourcePostionFor(f))

  def warn(m: String, f: File)(using CompilerContext): Unit =
    r.warning(createMessage(m, f, null), sourcePostionFor(f))


case class DocContext(args: Scala3doc.Args, compilerContext: CompilerContext)
  extends DokkaConfiguration:
    override def getOutputDir: File = args.output
    override def getCacheRoot: File = null
    override def getOfflineMode: Boolean = false
    override def getFailOnWarning: Boolean = false
    override def getSourceSets: JList[DokkaSourceSet] = JList(mkSourceSet)
    override def getModules: JList[DokkaConfiguration.DokkaModuleDescription] = JNil
    override def getPluginsClasspath: JList[File] = JNil
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

    val externalDocumentationLinks: List[Scala3docExternalDocumentationLink] = List(
      Scala3docExternalDocumentationLink(
        List(raw".*scala\/quoted.*".r),
        new URL("http://127.0.0.1:5500/scala3doc/output/scala3/"),
        DocumentationKind.Scala3doc
      ).withPackageList(new URL("http://127.0.0.1:5500/scala3doc/output/scala3/-scala%203/package-list")),
      Scala3docExternalDocumentationLink(
        List(raw".*java.*".r),
        new URL("https://docs.oracle.com/javase/8/docs/api/"),
        DocumentationKind.Javadoc
      ).withPackageList(new URL("https://docs.oracle.com/javase/8/docs/api/package-list")),
      Scala3docExternalDocumentationLink(
        List(raw".*scala.*".r),
        new URL("https://www.scala-lang.org/api/current/"),
        DocumentationKind.Scaladoc
      )
    )

    override def getPluginsConfiguration: JList[DokkaConfiguration.PluginConfiguration] =
      JNil

    val mkSourceSet: DokkaSourceSet =
      new DokkaSourceSetImpl(
        /*displayName=*/ args.name,
        /*sourceSetID=*/ new DokkaSourceSetID(args.name, "main"),
        /*classpath=*/ JNil,
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
