package test
import org.junit.{Test, Rule}
import org.junit.Assert._
import org.junit.rules.ErrorCollector
import org.jetbrains.dokka.testApi.testRunner.AbstractCoreTest$TestBuilder
import scala.io.Source
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.pages.ContentNodesKt
import org.jetbrains.dokka._
import scala.jdk.CollectionConverters._

class SignatureTests extends DottyAbstractCoreTest():

    val _collector = new ErrorCollector();

    @Rule
    def collector = _collector

    def parseSource(s: Source) : List[String] = 
        s.getLines().map(_.trim).filterNot(_.isBlank).filterNot(_.startsWith("=")).filterNot(_.startsWith(":")).toList
        
    def toScalaSeq[T](l: java.util.Collection[T]) = l.asScala.toSeq

    def flattenToText(node: ContentNode) : String = 
        def recursion(node: ContentNode, restriction: DokkaConfiguration$DokkaSourceSet) : Seq[ContentText] =
            node match {
                case t: ContentText => Seq(t)
                case c: ContentComposite => 
                    if(node.getDci.getKind != ContentKind.Annotations) 
                        toScalaSeq(c.getChildren)
                        .filter(_.getSourceSets.contains(restriction))
                        .flatMap(recursion(_, restriction))
                    else Seq()
                case _ => Seq()
            }
        val restriction = toScalaSeq(node.getSourceSets)(0)
        recursion(node, restriction).map(_.getText).mkString
    
    def getAllContentPages(root: PageNode) : Seq[ContentPage] = 
            def recursion(pn: PageNode) : Seq[ContentPage] = 
                pn match{
                    case c:ContentPage => Seq(c) ++ toScalaSeq(c.getChildren).flatMap(recursion(_))
                    case default => toScalaSeq(default.getChildren).flatMap(recursion(_))
                }
            recursion(root)

    def matchSignature(s: String, signatureList: List[String]) = 
    /* There's assumption that symbols are named using letters and digits only (no Scala operators :/) to make it easier to find symbol name */
        val candidateNames = signatureList.map(s => s.substring(s.indexOf(' ') + 1)).map(_.takeWhile(_.isLetterOrDigit))
        val symbolName = s.substring(s.indexOf(' ') + 1).takeWhile(_.isLetterOrDigit)
        val index = candidateNames.indexOf(symbolName)
        try {
            assertTrue(signatureList(index) == s)
        } catch {
            case e: IndexOutOfBoundsException => {
                val t = AssertionError(s"Signature: $s not matched. No candidate found")
                t.setStackTrace(Array())
                collector.addError(t)
            }
            case e: AssertionError => {
                val t = AssertionError(s"Signature: $s not matched. Candidate was ${signatureList(index)}")
                t.setStackTrace(Array())
                collector.addError(t)
            }
        }

    @Test
    def classWithMethodsSignatureTest(): Unit = 
        val testedFile = "target/scala-0.25/classes/tests/signatureTest"
        val signatureList = parseSource(Source.fromFile("src/main/scala/tests/signatureTestSource.scala"))
        
        val func = (t: AbstractCoreTest$TestBuilder) => {
            t.setPagesTransformationStage(
                (root: RootPageNode) => {
                    val classPage = toScalaSeq(root.getChildren)
                        .flatMap(p => toScalaSeq(p.getChildren))
                        .find(pg => pg.getName == "signatureTestSource")
                        .get
                        .asInstanceOf[ContentPage]

                    getAllContentPages(classPage).map(p => 
                        org.jetbrains.dokka.pages.ContentNodesKt.dfs(
                            p.getContent, 
                            q => q.getDci.getKind == ContentKind.Symbol
                        )
                    ).map(
                        flattenToText(_)
                    ).foreach(matchSignature(_,signatureList))

                    kotlin.Unit.INSTANCE
                }
            )
        }

        runTest(
            testedFile,
            List(),
            func
        )

