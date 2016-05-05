package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model.internal._

class PackageStructure extends DottyTest {
  @Test def multipleCompilationUnits = {
    val source1 = new SourceFile(
      "<test>",
      """
      |package scala
      |
      |trait A
      """.stripMargin
    )

    val source2 = new SourceFile(
      "<test>",
      """
      |package scala
      |
      |trait B
      """.stripMargin
    )

    checkSources(source1 :: source2 :: Nil) { doc =>
      doc.packages("scala") match {
        case PackageImpl(_, List(tA, tB), _, _) =>
          assert(
            tA.name == "A" && tB.name == "B",
            s"trait A had name '${tA.name}' and trait B had name '${tB.name}'"
          )
        case _ => fail("Incorrect package structure after run")
      }
    }
  }


  @Test def multiplePackages = {
    val source1 = new SourceFile(
      "<test>",
      """
      |package scala
      |package collection
      |
      |trait A
      """.stripMargin)

    val source2 = new SourceFile(
      "<test>",
      """
      |package scala
      |package collection
      |
      |trait B
      """.stripMargin)

    checkSources(source1 :: source2 :: Nil) { doc =>
      doc.packages("scala") match {
        case PackageImpl(
          "scala",
          List(PackageImpl("scala.collection", List(tA, tB), _, _)),
          _, _
        ) =>
          assert(
            tA.name == "A" && tB.name == "B",
            s"trait A had name '${tA.name}' and trait B had name '${tB.name}'"
          )

        case _ =>
          fail(s"""Incorrect package structure for 'scala' package: ${doc.packages("scala")}""")
      }

      doc.packages("scala.collection") match {
        case PackageImpl("scala.collection", List(tA, tB), _, _) =>
          assert(
            tA.name == "A" && tB.name == "B",
            s"trait A had name '${tA.name}' and trait B had name '${tB.name}'"
          )

        case _ => fail("Incorrect package structure for 'scala.collection' package")
      }
    }
  }
}
