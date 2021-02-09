package dotty.tools.scaladoc
package diagram

import dotty.tools.scaladoc.ScaladocTest
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}

class HierarchyTest extends ScaladocTest("hierarchy"):
  override def runTest = withModule(_.visitMembers(checkMember))

  def checkMember(x: Member) = x.name match
    case "C1" =>
      assertEquals(List("A1", "A2[Int]", "A3[Int, String]", "Any", "B1", "B2", "B3", "Matchable", "Object"), x.getParentsAsStrings)
        assertEquals(List("B1", "B2", "B3"), x.getDirectParentsAsStrings)
        assertEquals(List("E1", "E2"), x.getKnownChildrenAsStrings)
        assertTrue("Graph is empty!", x.graph != HierarchyGraph.empty)
        assertEquals(
          Set(
            "Object" -> "Matchable",
            "Matchable" -> "Any",
            "Object" -> "Any",
            "A1" -> "Object",
            "A2[Int]" -> "Object",
            "A3[Int, String]" -> "Object",
            "B1" -> "Object",
            "B1" -> "A1",
            "B2" -> "Object",
            "B2" -> "A1",
            "B2" -> "A2[Int]",
            "B3" -> "Object",
            "B3" -> "A2[Int]",
            "B3" -> "A3[Int, String]",
            "C1[A, B, C]" -> "Object",
            "C1[A, B, C]" -> "B1",
            "C1[A, B, C]" -> "B2",
            "C1[A, B, C]" -> "B3",
            "E1" -> "C1[A, B, C]",
            "E2" -> "C1[A, B, C]"
          ),
          x.graph.edges.map((a, b) => (a.signature.getName, b.signature.getName)).toSet
        )
    case "E2" =>
      assertEquals(List("A1", "A2[Int]", "A3[Int, String]","A4", "Any", "B1", "B2", "B3", "C1[Int, Boolean, Any]", "D2[Int, Boolean]", "D3", "Matchable", "Object"), x.getParentsAsStrings)
      assertEquals(List("C1[Int, Boolean, Any]", "D2[Int, Boolean]", "D3"), x.getDirectParentsAsStrings)
      assertEquals(List.empty, x.getKnownChildrenAsStrings)
      assertTrue("Graph is empty!", x.graph != HierarchyGraph.empty)
      assertEquals(
        Set(
          "Object" -> "Any",
          "A1" -> "Object",
          "A2[Int]" -> "Object",
          "A3[Int, String]" -> "Object",
          "A4" -> "Object",
          "B1" -> "Object",
          "B1" -> "A1",
          "B2" -> "Object",
          "B2" -> "A1",
          "B2" -> "A2[Int]",
          "B3" -> "Object",
          "B3" -> "A2[Int]",
          "B3" -> "A3[Int, String]",
          "C1[Int, Boolean, Any]" -> "Object",
          "C1[Int, Boolean, Any]" -> "B1",
          "C1[Int, Boolean, Any]" -> "B2",
          "C1[Int, Boolean, Any]" -> "B3",
          "Object" -> "Matchable",
          "Matchable" -> "Any",
          "E2" -> "D2[Int, Boolean]",
          "E2" -> "D3",
          "D2[Int, Boolean]" -> "Object",
          "D3" -> "A4",
          "D3" -> "Object",
          "E2" -> "C1[Int, Boolean, Any]",
        ),
        x.graph.edges.map((a, b) => (a.signature.getName, b.signature.getName)).toSet
      )
    case "A2" =>
      assertEquals(List("Any", "Matchable", "Object"), x.getParentsAsStrings)
      assertEquals(List.empty, x.getDirectParentsAsStrings)
      assertEquals(List("B2", "B3", "C1[A, B, C]", "E1", "E2"), x.getKnownChildrenAsStrings)
      assertTrue("Graph is empty!", x.graph != HierarchyGraph.empty)
      assertEquals(
        Set(
          "Object" -> "Matchable",
          "Matchable" -> "Any",
          "Object" -> "Any",
          "A2[T]" -> "Object",
          "B2" -> "A2[T]",
          "B3" -> "A2[T]",
          "C1[A, B, C]" -> "B2",
          "C1[A, B, C]" -> "B3",
          "E1" -> "C1[A, B, C]",
          "E2" -> "C1[A, B, C]",
        ),
        x.graph.edges.map((a, b) => (a.signature.getName, b.signature.getName)).toSet
      )
    case _ =>
