package dotty.dokka
import org.jetbrains.dokka.utilities.{DokkaConsoleLogger, DokkaLogger}
import testApi.logger.TestLogger
import org.jetbrains.dokka.testApi.testRunner._
import org.jetbrains.dokka.plugability.DokkaPlugin
import java.io.File
import dotty.tastydoc.representations
import dotty.tastydoc.representations._
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

abstract class DottyAbstractCoreTest extends AbstractCoreTest:
    private def getTempDir() : TemporaryFolder =
        val folder = new TemporaryFolder()
        folder.create()
        folder

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
            val (files, dirs) = f.listFiles().partition(_.isFile)
            files.filter(_.getName.endsWith(".tasty")) ++ dirs.flatMap(listTastyFiles)

        val tastyFiles = tastyDir.split(File.pathSeparatorChar).toList.flatMap(p => listTastyFiles(new File(p))).map(_.toString)
            
        val config = new DottyDokkaConfig(
            DocConfiguration(
                tastyFiles = tastyFiles,
                classpath = System.getProperty("java.class.path")
            )
        )
        config._outputDir = getTempDir().getRoot.toPath.toAbsolutePath.toString
        DokkaTestGenerator(
            config,
            new TestLogger(DokkaConsoleLogger.INSTANCE),
            tests.build(),
            Nil.asJava
        ).generate()    

        signatures

    def signaturesFromDocumentation(tastyDir: String): Seq[String] = 
        def flattenToText(node: ContentNode) : Seq[ContentText] = node match
            case t: ContentText => Seq(t)
            case c: ContentComposite => 
                if node.getDci.getKind == ContentKind.Annotations then Seq()
                else c.getChildren.asScala.flatMap(flattenToText).toSeq
            case _ => Seq()
    
        def all(p: ContentNode => Boolean)(n: ContentNode): Seq[ContentNode] =
            if p(n) then Seq(n) else n.getChildren.asScala.toSeq.flatMap(all(p))
            

        val pages = listPages(tastyDir)
        val nodes = pages.flatMap(p => all(_.getDci.getKind == ContentKind.Symbol)(p.getContent))
        nodes.map(flattenToText(_).map(_.getText).mkString)

    def signaturesFromSource(s: Source): Seq[String] =
        val ExpectRegex = ".+//expect: (.+)".r
            // e.g. to removes '(0)' from object IAmACaseObject extends CaseImplementThis/*<-*/(0)/*->*/ 
        val CommentRegexp = """\/\*<-\*\/[^\/]+\/\*->\*\/"""

        def (s: String).doesntStartWithAnyOfThese(c: Char*) = c.forall(char => !s.startsWith(char.toString))
        val lines = s.getLines().map(_.trim).toList
            .filter(_.doesntStartWithAnyOfThese('=',':','{','}'))
            .filterNot(_.trim.isEmpty)
            .filterNot(_.startsWith("//"))
        
        lines.map {
            case ExpectRegex(signature) => signature
            case other =>
                other.replaceAll(CommentRegexp, "").replaceAll("  +", " ")

        } 
        
    val _collector = new ErrorCollector();
    @Rule
    def collector = _collector
    def reportError(msg: String) = collector.addError(new AssertionError(msg))
