package dotty.tools.scaladoc
package diagram

import dotty.tools.scaladoc.ScaladocTest
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}
import org.junit.Test

class SealedHierarchyTest extends ScaladocTest("sealedClasses"):
  @Test
  def runTest(): Unit = withModule(_.visitMembers(checkMember))

  def checkMember(x: Member) = x.name match
    case "A" =>
      assertEquals(4, x.graph.sealedNodes.size)
      assertEquals(
        Set("A", "B1", "C1", "C7"),
        x.graph.sealedNodes.map(_.signature.getName)
      )
    case "B1" =>
      assertEquals(4, x.graph.sealedNodes.size)
      assertEquals(
        Set("A", "B1", "C1", "C7"),
        x.graph.sealedNodes.map(_.signature.getName)
      )
    case "C1" =>
      assertEquals(3, x.graph.sealedNodes.size)
      assertEquals(
        Set("A", "B1", "C1"),
        x.graph.sealedNodes.map(_.signature.getName)
      )
    case _ =>
