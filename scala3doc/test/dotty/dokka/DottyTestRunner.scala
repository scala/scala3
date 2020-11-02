package dotty.dokka
import org.jetbrains.dokka.utilities.{DokkaConsoleLogger, DokkaLogger}
import org.jetbrains.dokka.testApi.logger.TestLogger
import org.jetbrains.dokka.testApi.testRunner._
import org.jetbrains.dokka.plugability.DokkaPlugin
import java.io.File
import dotty.dokka.{DocConfiguration, DottyDokkaConfig}
import collection.JavaConverters._
import org.junit.rules.TemporaryFolder
import org.junit.{Test, Rule}
import org.junit.Assert._
import org.junit.rules.ErrorCollector
import org.jetbrains.dokka.testApi.testRunner.AbstractCoreTest$TestBuilder
import scala.io.Source
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.pages.ContentNodesKt
import org.jetbrains.dokka._
import collection.JavaConverters._
import scala.math.max
import org.jetbrains.dokka.pages.ContentNodesKt
import dotty.dokka.model.api.Link

abstract class DottyAbstractCoreTest extends AbstractCoreTest:
  private def getTempDir() : TemporaryFolder =
    val folder = new TemporaryFolder()
    folder.create()
    folder

  private def args = Args(
    name = "test",
    tastyRoots = Nil ,
    classpath =  System.getProperty("java.class.path"),
    None,
    output = getTempDir().getRoot,
    projectVersion = "1.0",
    projectTitle = None,
    projectLogo = None,
    defaultSyntax = None,
    sourceLinks = List.empty
  )

  def listPages(tastyDir: String): Seq[ContentPage] =
    var signatures: Seq[ContentPage] = Nil
    val tests = new AbstractCoreTest$TestBuilder()


    def getAllContentPages(root: PageNode) : Seq[ContentPage] = root match
      case c: ContentPage => Seq(c) ++ c.getChildren.asScala.flatMap(getAllContentPages)
      case default => default.getChildren.asScala.toSeq.flatMap(getAllContentPages)

    tests.setPagesTransformationStage { root =>
      val res = root.getChildren.asScala.flatMap(getAllContentPages)
      signatures = res.toSeq
      kotlin.Unit.INSTANCE
    }

    def listTastyFiles(f: File): Seq[File] =
      assertTrue(s"Tasty root dir does not exisits: $f", f.isDirectory())
      val (files, dirs) = f.listFiles().partition(_.isFile)
      files.toIndexedSeq.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)

    val tastyFiles = tastyDir.split(File.pathSeparatorChar).toList.flatMap(p => listTastyFiles(new File(p))).map(_.toString)

    val config = new DottyDokkaConfig(DocConfiguration.Standalone(args, tastyFiles, Nil))
    DokkaTestGenerator(
      config,
      new TestLogger(DokkaConsoleLogger.INSTANCE),
      tests.build(),
      Nil.asJava
    ).generate()

    signatures

  def signaturesFromDocumentation(tastyDir: String): Seq[String] =
    def flattenToText(node: ContentNode) : Seq[String] = node match
      case t: ContentText => Seq(t.getText)
      case c: ContentComposite =>
        c.getChildren.asScala.flatMap(flattenToText).toSeq
      case l: DocumentableElement =>
        (l.annotations ++ Seq(" ") ++ l.modifiers ++ Seq(l.name) ++ l.signature).map {
          case s: String => s
          case Link(s: String, _) => s
        }
      case _ => Seq()

    def all(p: ContentNode => Boolean)(n: ContentNode): Seq[ContentNode] =
      if p(n) then Seq(n) else n.getChildren.asScala.toSeq.flatMap(all(p))


    val pages = listPages(tastyDir)
    val nodes = pages.flatMap(p => all(_.isInstanceOf[DocumentableElement])(p.getContent))
    nodes.map(flattenToText(_).mkString.trim)

  def signaturesFromSource(s: Source): SignaturesFromSource =
    val ExpectedRegex = ".+//expected: (.+)".r
    val UnexpectedRegex = "(.+)//unexpected".r

    // e.g. to remove '(0)' from object IAmACaseObject extends CaseImplementThis/*<-*/(0)/*->*/
    val CommentRegexp = """\/\*<-\*\/[^\/]+\/\*->\*\/"""

    extension (s: String) def doesntStartWithAnyOfThese(c: Char*) = c.forall(char => !s.startsWith(char.toString))
    val lines = s.getLines().map(_.trim).toList
      .filter(_.doesntStartWithAnyOfThese('=',':','{','}'))
      .filterNot(_.trim.isEmpty)
      .filterNot(_.startsWith("//"))

    val expectedSignatures = lines.flatMap {
      case UnexpectedRegex(_) => None
      case ExpectedRegex(signature) => Some(signature)
      case other =>
        Some(other.replaceAll(CommentRegexp, "").replaceAll("  +", " "))
    }

    val unexpectedSignatures = lines.collect {
      case UnexpectedRegex(signature) => signature.trim
    }

    SignaturesFromSource(expectedSignatures, unexpectedSignatures)

  val _collector = new ErrorCollector();
  @Rule
  def collector = _collector
  def reportError(msg: String) = collector.addError(new AssertionError(msg))


case class SignaturesFromSource(expected: Seq[String], unexpected: Seq[String])
