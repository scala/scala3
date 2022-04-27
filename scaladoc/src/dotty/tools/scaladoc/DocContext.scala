package dotty.tools.scaladoc

import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import collection.JavaConverters._
import dotty.tools.scaladoc.site.StaticSiteContext
import dotty.tools.dotc.core.Contexts._
import dotty.tools.io.VirtualFile
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.io.Codec
import java.net.URL
import scala.util.Try
import scala.collection.mutable
import dotty.tools.scaladoc.util.Check.checkJekyllIncompatPath

type CompilerContext = dotty.tools.dotc.core.Contexts.Context

given compilerContext(using docContext: DocContext): CompilerContext =
  docContext.compilerContext

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
    val sourceFile = SourceFile.virtual(relPath.toString, content = "")
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

  def warn(m: String, e: Throwable)(using CompilerContext): Unit =
    r.warning(s"$m: ${throwableToString(e)}")

  def echo(m: String)(using CompilerContext): Unit =
    r.echo(m)

case class NavigationNode(name: String, dri: DRI, nested: Seq[NavigationNode])

case class DocContext(args: Scaladoc.Args, compilerContext: CompilerContext):
  lazy val sourceLinks = SourceLinks.load(args.sourceLinks, args.revision)(using compilerContext)

  lazy val commentSyntaxArgs = tasty.comments.CommentSyntaxArgs.load(args.defaultSyntax)(using compilerContext)

  lazy val snippetCompilerArgs = snippets.SnippetCompilerArgs.load(args.snippetCompiler)(using compilerContext)

  lazy val snippetChecker = snippets.SnippetChecker(args)(using compilerContext)

  lazy val staticSiteContext = args.docsRoot.map(path => StaticSiteContext(
      File(path).getAbsoluteFile(),
      args,
      sourceLinks,
      snippetCompilerArgs,
      snippetChecker
    )(using compilerContext))


  val externalDocumentationLinks = args.externalMappings


  // We collect and report any generated files incompatible with Jekyll
  private lazy val jekyllIncompatLinks = mutable.HashSet[String]()

  def checkPathCompat(path: Seq[String]): Unit =
    if checkJekyllIncompatPath(path) then jekyllIncompatLinks.add(path.mkString("/"))

  def reportPathCompatIssues(): Unit =
    if !jekyllIncompatLinks.isEmpty then
      report.warning(
        s"""Following generated file paths might not be compatible with Jekyll:
          |${jekyllIncompatLinks.mkString("\n")}
          |If using GitHub Pages consider adding a \".nojekyll\" file.
        """.stripMargin)(using compilerContext)
