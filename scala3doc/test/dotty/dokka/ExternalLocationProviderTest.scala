package dotty.dokka

import dotty.dokka.tasty._

import scala.collection.JavaConverters._
import java.nio.file.Paths
import java.nio.file.Path
import scala.util.matching._
import dotty.dokka.model.api._
import java.net.URL
import org.junit.{Test}
import org.junit.Assert._

import scala.quoted._

class ExternalLocationProviderTest
  // TODO rewrite for locations!

//   def createExternalLocationProvider(docURL: String, ext: String, kind: DocumentationKind) = {
//     val emptyExtDoc = ED(
//           URL(docURL),
//           PackageList(
//             RecognizedLinkFormat.Javadoc1, JSet(), JMap(), URL(docURL)
//           )
//         )
//     ScalaExternalLocationProvider(emptyExtDoc, ext, kind)
//   }

//   def testResolvedLinks(provider: ScalaExternalLocationProvider, testcases: List[(DRI, String)]) = testcases.foreach {
//     case (dri, expect) => assertEquals(provider.resolve(dri), expect)
//   }

//   @Test
//   def javadocExternalLocationProviderTest(): Unit = {
//     val provider = createExternalLocationProvider("https://docs.oracle.com/javase/8/docs/api/", ".html", DocumentationKind.Javadoc)
//     val testcases = List(
//       (DRI("java.util.Map$$Entry"), "https://docs.oracle.com/javase/8/docs/api/java/util/Map.Entry.html"),
//       (DRI("javax.swing.plaf.nimbus.AbstractRegionPainter$$PaintContext$$CacheMode"), "https://docs.oracle.com/javase/8/docs/api/javax/swing/plaf/nimbus/AbstractRegionPainter.PaintContext.CacheMode.html"),
//       (DRI("java.lang.CharSequence"), "https://docs.oracle.com/javase/8/docs/api/java/lang/CharSequence.html")
//     )
//     testResolvedLinks(provider, testcases)
//   }

//   @Test
//   def scaladocExternalLocationProviderTest(): Unit = {
//     val provider = createExternalLocationProvider("https://www.scala-lang.org/api/current/", ".html", DocumentationKind.Scaladoc)
//     val testcases = List(
//       (DRI("scala.Predef$"),"https://www.scala-lang.org/api/current/scala/Predef$.html"),
//       (DRI("scala.util.package$$chaining$"), "https://www.scala-lang.org/api/current/scala/util/package$$chaining$.html"),
//       (DRI("scala.util.Using$"), "https://www.scala-lang.org/api/current/scala/util/Using$.html"),
//       (DRI("scala.util.matching.Regex$$Match"), "https://www.scala-lang.org/api/current/scala/util/matching/Regex$$Match.html")
//     )
//     testResolvedLinks(provider, testcases)
//   }

//   @Test
//   def scala3docExternalLocationProviderTest(): Unit = {
//     val provider = createExternalLocationProvider("https://dotty.epfl.ch/api/", ".html", DocumentationKind.Scala3doc)
//     val testcases = List(
//       (DRI("scala.Predef$"),"https://dotty.epfl.ch/api/scala/Predef$.html"),
//       (DRI("scala.util.package$$chaining$"), "https://dotty.epfl.ch/api/scala/util/package$$chaining$.html"),
//       (DRI("scala.util.Using$"), "https://dotty.epfl.ch/api/scala/util/Using$.html"),
//       (DRI("scala.util.matching.Regex$$Match"), "https://dotty.epfl.ch/api/scala/util/matching/Regex$$Match.html")
//     )
//     testResolvedLinks(provider, testcases)
//   }