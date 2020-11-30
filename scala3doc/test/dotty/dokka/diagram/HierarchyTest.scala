package dotty.dokka.diagram

import dotty.dokka.ScaladocTest
import dotty.dokka.Assertion.AfterDocumentablesTransformation
import dotty.dokka.kUnit
import dotty.dokka.model.api._
import scala.jdk.CollectionConverters.{ListHasAsScala, SeqHasAsJava}
import org.junit.Assert.{assertSame, assertTrue, assertEquals}

class HierarchyTest extends ScaladocTest("hierarchy"):
    override def assertions = Seq(
        AfterDocumentablesTransformation { m =>
            m.visitMembers { x =>
                if (x.getName == "C1") {
                    assertEquals(List("A1", "A2[Int]", "A3[Int, String]", "Any", "B1", "B2", "B3", "Object"), x.getParentsAsStrings)
                    assertEquals(List("B1", "B2", "B3"), x.getDirectParentsAsStrings)
                    assertEquals(List("E1", "E2"), x.getKnownChildrenAsStrings)
                    val graph = MemberExtension.getFrom(x).map(_.graph)     
                    assertEquals(true, graph.isDefined)
                    assertEquals(
                        Seq(
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
                        ).sorted,
                        graph.get.edges.map((a, b) => (a.signature.getName, b.signature.getName)).sorted
                    )
                }
                if (x.getName == "E2") {
                    assertEquals(List("A1", "A2[Int]", "A3[Int, String]", "Any", "B1", "B2", "B3", "C1[Int, Boolean, Any]", "D2[Int, Boolean]", "D3", "Object"), x.getParentsAsStrings)
                    assertEquals(List("C1[Int, Boolean, Any]", "D2[Int, Boolean]", "D3"), x.getDirectParentsAsStrings)
                    assertEquals(List.empty, x.getKnownChildrenAsStrings)
                    val graph = MemberExtension.getFrom(x).map(_.graph)   
                    assertEquals(true, graph.isDefined)
                    assertEquals(
                        Seq(
                            "Object" -> "Any",
                            // "A1" -> "Object", // These are not applicable beacuase of bug and its workaround 
                            // "A2[Int]" -> "Object", // More info at ClassLikeSupport.scala:37
                            // "A3[Int, String]" -> "Object",
                            // "B1" -> "Object",
                            // "B1" -> "A1",
                            // "B2" -> "Object",
                            // "B2" -> "A1",
                            // "B2" -> "A2[Int]",
                            // "B3" -> "Object",
                            // "B3" -> "A2[Int]",
                            // "B3" -> "A3[Int, String]",
                            // "C1[Int, Boolean, Any]" -> "Object",
                            // "C1[Int, Boolean, Any]" -> "B1",
                            // "C1[Int, Boolean, Any]" -> "B2",
                            // "C1[Int, Boolean, Any]" -> "B3",
                            "E2" -> "D2[Int, Boolean]",
                            "E2" -> "D3",
                            "D2[Int, Boolean]" -> "Object",
                            "D3" -> "Object",
                            "E2" -> "C1[Int, Boolean, Any]"
                        ).sorted,
                        graph.get.edges.map((a, b) => (a.signature.getName, b.signature.getName)).sorted
                    )
                }
                if (x.getName == "A2") {
                    assertEquals(List("Any", "Object"), x.getParentsAsStrings)
                    assertEquals(List.empty, x.getDirectParentsAsStrings)
                    assertEquals(List("B2", "B3", "C1[A, B, C]", "E1", "E2"), x.getKnownChildrenAsStrings)
                    val graph = MemberExtension.getFrom(x).map(_.graph)   
                    assertEquals(true, graph.isDefined)
                    assertEquals(
                        Seq(
                            "Object" -> "Any",
                            "A2[T]" -> "Object",
                            "B2" -> "A2[T]", // These are not actually true, becuase we lose information about hierarchy in subtypes and their possible mapping to supertypes other that that type itself, e. g. linking to `Object` 
                            "B3" -> "A2[T]",
                            "C1[A, B, C]" -> "A2[T]",
                            "E1" -> "A2[T]",
                            "E2" -> "A2[T]"
                        ).sorted,
                        graph.get.edges.map((a, b) => (a.signature.getName, b.signature.getName)).sorted
                    )
                }
            }
        }
    ) 
