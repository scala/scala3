package dotty.dokka.linking

import scala.jdk.CollectionConverters._
import scala.Function.const
import org.jetbrains.dokka.links.DRI
import org.jetbrains.dokka.model.DModule
import dotty.dokka.model.api._
import dotty.dokka.{ScaladocTest, Assertion}

abstract class DriTest(testName: String) extends ScaladocTest(testName):
  // override for additional assertions
  def assertOnDRIs(dris: Seq[DRI]): Unit = ()

  override def assertions = Assertion.AfterDocumentablesTransformation { root =>
    val dris = root.collectMembers.map(_.dri)

    val grouping = dris.groupMapReduce(identity)(const(1))(_+_)
    val duplicates = grouping.filter { (_, v )=> v > 1 }

    if duplicates.nonEmpty then
      val duplicatesMessage = duplicates.map { (k, v) => s"$k - $v times" }.mkString("\n")
      val otherMessage = grouping.flatMap { (k, v) => Option.when(v == 1)(k) }.mkString("\n")
      val message = s"\nThere were some repeating DRIs:\n$duplicatesMessage\n\nOther DRIs:\n$otherMessage\n\n"
      reportError(message)

    assertOnDRIs(dris)
  } :: Nil

extension (m: DModule) private def collectMembers = m.getPackages.asScala.toList.flatMap(collectFrom)

private def collectFrom(m: Member): Seq[Member] =
  m +: m.allMembers.filter(_.origin == Origin.DefinedWithin).flatMap(collectFrom)
