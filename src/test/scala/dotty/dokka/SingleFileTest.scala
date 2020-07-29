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

object SingleFileTest {
  val classlikeKinds = Seq("class",  "object", "trait") // TODO add docs for packages
  val members = Seq("type", "def", "val", "var")
  val all = classlikeKinds ++ members
}

abstract class SingleFileTest(val fileName: String, signatureKinds: Seq[String], ignoreUndocumented: Boolean = false) extends DottyAbstractCoreTest:
  private val _collector = new ErrorCollector();

  /* There's assumption that symbols are named using letters and digits only (no Scala operators :/) to make it easier to find symbol name */
  def extractSymbolName(signature: String) = 
      val helper = signatureKinds
        .map(_ + " ") // We want to have at least one space after kind however why code below does not work!!
        .maxBy(p => signature.indexOf("xxxxx")) // is this a bug in dotty?
      
      val helperIndex = signature.indexOf(helper)

      if  helperIndex == -1 then "NULL" 
      else signature.substring(helperIndex + helper.size + 1).takeWhile(_.isLetterOrDigit)

  def matchSignature(s: String, signatureList: List[String]): Seq[String] = 
      val symbolName = extractSymbolName(s)
      val candidates = signatureList.filter(extractSymbolName(_) == symbolName)

      candidates.filter(_ == s) match {
          case Nil =>
              val candidateMsg = 
                  if candidates.isEmpty then s"No candidate found for symbol name $symbolName"
                  else s"Candidates:\n${candidates.mkString("\n")}\n"

              reportError(s"No match for:\n$s\n$candidateMsg")
              Nil
          case matching =>
              matching
      }

  @Test
  def testSignatures(): Unit = 
      val allFromSource = signaturesFromSource(Source.fromFile(s"src/main/scala/tests/$fileName.scala")).toList
      val fromSource = allFromSource.filter(extractSymbolName(_) != "NULL")

      val allFromDocumentation = signaturesFromDocumentation(s"target/scala-0.25/classes/tests/$fileName")
      val fromDocumentation = allFromDocumentation.filter(extractSymbolName(_) != "NULL")
      
      val documentedSignature = fromDocumentation.flatMap(matchSignature(_, fromSource)).toSet
      val missingSignatures = fromDocumentation.filterNot(documentedSignature.contains)
      
      if !ignoreUndocumented && missingSignatures.nonEmpty then reportError(
           s"""Not documented signatures:\n ${missingSignatures.mkString("\n")} \n All signatures: ${fromDocumentation.mkString("\n")}"""
      )