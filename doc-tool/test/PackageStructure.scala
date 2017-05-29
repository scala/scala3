package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model.internal._

class PackageStructure extends DottyDocTest {
  @Test def multipleCompilationUnits = {
    val source1 = new SourceFile(
      "TraitA.scala",
      """
      |package scala
      |
      |trait A
      """.stripMargin
    )

    val source2 = new SourceFile(
      "TraitB.scala",
      """
      |package scala
      |
      |trait B
      """.stripMargin
    )

    checkSources(source1 :: source2 :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(tA, tB), _, _, _, _) =>
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
      "TraitA.scala",
      """
      |package scala
      |package collection
      |
      |trait A
      """.stripMargin)

    val source2 = new SourceFile(
      "TraitB.scala",
      """
      |package scala
      |package collection
      |
      |trait B
      """.stripMargin)

    checkSources(source1 :: source2 :: Nil) { packages =>
      packages("scala.collection") match {
        case PackageImpl(_, _, "scala.collection", List(tA, tB), _, _, _, _) =>
          assert(
            tA.name == "A" && tB.name == "B",
            s"trait A had name '${tA.name}' and trait B had name '${tB.name}'"
          )

        case _ =>
          fail(s"""Incorrect package structure for 'scala' package: ${packages("scala")}""")
      }

      packages("scala.collection") match {
        case PackageImpl(_, _, "scala.collection", List(tA, tB), _, _, _, _) =>
          assert(
            tA.name == "A" && tB.name == "B",
            s"trait A had name '${tA.name}' and trait B had name '${tB.name}'"
          )

        case _ => fail("Incorrect package structure for 'scala.collection' package")
      }
    }
  }
}
