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

abstract class SingleFileTest(val fileName: String, signatureKinds: Seq[String], ignoreUndocumented: Boolean = false) extends MultipleFileTest(
    List(fileName),
    List(fileName),
    signatureKinds,
    ignoreUndocumented
)

