package dotty.dokka

import org.junit.{Test, Rule}
import org.junit.Assert._
import org.junit.rules.ErrorCollector
import org.jetbrains.dokka.testApi.testRunner.AbstractCoreTest$TestBuilder
import scala.io.Source
import org.jetbrains.dokka.pages._
import org.jetbrains.dokka.pages.ContentNodesKt
import org.jetbrains.dokka._
import scala.jdk.CollectionConverters._
import scala.math.max

object MultipleFileTest{
    val classlikeKinds = Seq("class",  "object", "trait") // TODO add docs for packages
    val members = Seq("type", "def", "val", "var")
    val all = classlikeKinds ++ members
}

abstract class MultipleFileTest(val sourceFiles: List[String], val tastyFolders: List[String] ,signatureKinds: Seq[String], ignoreUndocumented: Boolean = false) extends DottyAbstractCoreTest:
    private val _collector = new ErrorCollector();

  /* There's assumption that symbols are named using letters and digits only (no Scala operators :/) to make it easier to find symbol name */
    def extractSymbolName(signature: String) = 
        val helper = signatureKinds.find(k => signature.contains(s"$k "))
        
        helper.fold("NULL"){ h =>
            val helperIndex = signature.indexOf(h)
            signature.substring(helperIndex + helper.size + 1).takeWhile(_.isLetterOrDigit)
        }  
        
    def matchSignature(s: String, signatureList: List[String]): Seq[String] = 
        val symbolName = extractSymbolName(s)
        val candidates = signatureList.filter(extractSymbolName(_) == symbolName)

        candidates.filter(_ == s) match {
            case Nil =>
                val candidateMsg = 
                    if candidates.isEmpty then s"No candidate found for symbol name $symbolName"
                    else s"Candidates:\n${candidates.mkString("\n")}\n"

                //reportError(s"No match for:\n$s\n$candidateMsg") All test would fail because of documented inherited methods
                println(s"No match for:\n$s\n$candidateMsg")
                Nil
            case matching =>
                matching
        }

    @Test
    def testSignatures(): Unit = 
        def cleanup(s: String) = s.replace("\n", " ").replaceAll(" +", " ")
        val allFromSource = sourceFiles.flatMap(file => signaturesFromSource(Source.fromFile(s"src/main/scala/tests/$file.scala")).toList)
        val fromSource = allFromSource.filter(extractSymbolName(_) != "NULL").map(cleanup)

        val allFromDocumentation = tastyFolders.flatMap(folder => signaturesFromDocumentation(s"target/scala-0.26/classes/tests/$folder"))
        val fromDocumentation = allFromDocumentation.filter(extractSymbolName(_) != "NULL").map(cleanup)
        
        val documentedSignature = fromDocumentation.flatMap(matchSignature(_, fromSource)).toSet
        val missingSignatures = fromSource.filterNot(documentedSignature.contains)
        
        if !ignoreUndocumented && missingSignatures.nonEmpty then reportError(
            s"""Not documented signatures:\n ${missingSignatures.mkString("\n")} \n All signatures: ${fromDocumentation.mkString("\n")}"""
        )

