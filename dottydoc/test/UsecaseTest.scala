package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model._
import model.internal._
import model.references._

class UsecaseTest extends DottyTest {
  @Test def simpleUsecase = {
    val source = new SourceFile(
      "DefWithUseCase.scala",
      """
      |package scala
      |
      |trait Test[A] {
      |  /** Definition with a "disturbing" signature
      |   *
      |   *  @usecase def foo: A
      |   */
      |  def foo[B]: A => B
      |}
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, _, List(trt: Trait), _, _) =>
          val List(map: Def) = trt.members

          val returnValue = map.returnValue match {
            case ref: TypeReference => ref.title
            case _ =>
              assert(
                false,
                "Incorrect return value after usecase transformation"
              )
              ""
          }

          assert(
            map.typeParams.isEmpty,
            "Type parameters were not stripped by usecase"
          )
          assert(returnValue == "A", "Incorrect return type after usecase")
      }
    }
  }
}
