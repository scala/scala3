package dotty.tools.scaladoc
package linking

import scala.jdk.CollectionConverters._
import scala.Function.const
import dotty.tools.scaladoc.ScaladocTest

abstract class DriTest(testName: String) extends ScaladocTest(testName):
  // override for additional assertions
  def assertOnDRIs(dris: Seq[DRI]): Unit = ()

  override def runTest = withModule { module =>
    val dris = module.members.keys.toSeq

    val grouping = dris.groupMapReduce(identity)(const(1))(_+_)
    val duplicates = grouping.filter { (_, v )=> v > 1 }

    if duplicates.nonEmpty then
      val duplicatesMessage = duplicates.map { (k, v) => s"$k - $v times" }.mkString("\n")
      val otherMessage = grouping.flatMap { (k, v) => Option.when(v == 1)(k) }.mkString("\n")
      val message = s"\nThere were some repeating DRIs:\n$duplicatesMessage\n\nOther DRIs:\n$otherMessage\n\n"
      reportError(message)

    assertOnDRIs(dris)
  } :: Nil


private def collectFrom(m: Member): Seq[Member] =
  m +: m.members.filter(_.origin == Origin.RegularlyDefined).flatMap(collectFrom)
