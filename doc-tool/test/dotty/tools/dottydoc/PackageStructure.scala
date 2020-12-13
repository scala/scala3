package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._
import dotc.util.SourceFile
import model.Trait
import model.internal._

class PackageStructureFromSourceTest extends PackageStructureBase with CheckFromSource
class PackageStructureFromTastyTest extends PackageStructureBase with CheckFromTasty

abstract class PackageStructureBase extends DottyDocTest {

  @Test def sourceFileAnnotIsStripped = {
    val source = SourceUtil.makeTemp(
      """package scala
        |
        |/** Some doc */
        |trait A
      """.stripMargin
    )

    val tastyFile = "scala/A.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
          assert(trt.annotations.isEmpty)
      }
    }
  }

  @Test def multipleCompilationUnits = {
    val source1 = SourceUtil.makeTemp(
      """
      |package scala
      |
      |trait A
      """.stripMargin
    )

    val source2 = SourceUtil.makeTemp(
      """
      |package scala
      |
      |trait B
      """.stripMargin
    )

    val tastyFiles = "scala/A.tasty" :: "scala/B.tasty" :: Nil

    check(tastyFiles, source1 :: source2 :: Nil) { (ctx, packages) =>
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
    val source1 = SourceUtil.makeTemp(
      """
      |package scala
      |package collection
      |
      |trait A
      """.stripMargin)

    val source2 = SourceUtil.makeTemp(
      """
      |package scala
      |package collection
      |
      |trait B
      """.stripMargin)

    val tastyFiles = "scala/collection/A.tasty" :: "scala/collection/B.tasty" :: Nil

    check(tastyFiles, source1 :: source2 :: Nil) { (ctx, packages) =>
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
