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
          val List(foo: Def) = trt.members

          val returnValue = foo.returnValue match {
            case ref: TypeReference => ref.title
            case _ =>
              assert(
                false,
                "Incorrect return value after usecase transformation"
              )
              ""
          }

          assert(
            foo.typeParams.isEmpty,
            "Type parameters were not stripped by usecase"
          )
          assert(returnValue == "A", "Incorrect return type after usecase")

          assert(foo.name == "foo", s"Incorrect name after transform: ${foo.name}")
      }
    }
  }

  @Test def simpleUsecaseAddedArg = {
    val source = new SourceFile(
      "DefWithUseCase.scala",
      """
      |package scala
      |
      |trait Test[A] {
      |  /** Definition with a "disturbing" signature
      |   *
      |   *  @usecase def foo(a: A): A
      |   */
      |  def foo[B]: A => B
      |}
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, _, List(trt: Trait), _, _) =>
          val List(foo: Def) = trt.members

          val returnValue = foo.returnValue match {
            case ref: TypeReference => ref.title
            case _ =>
              assert(
                false,
                "Incorrect return value after usecase transformation"
              )
              ""
          }

          assert(
            foo.typeParams.isEmpty,
            "Type parameters were not stripped by usecase"
          )
          assert(returnValue == "A", "Incorrect return type after usecase")
          assert(
            foo.paramLists.head.list.head.title == "a",
            "Incorrect parameter to function after usecase transformation"
          )
          assert(foo.name == "foo", s"Incorrect name after transform: ${foo.name}")
      }
    }
  }

  @Test def simpleTparamUsecase = {
    val source = new SourceFile(
      "DefWithUseCase.scala",
      """
      |package scala
      |
      |trait Test[A] {
      |  /** Definition with a "disturbing" signature
      |   *
      |   *  @usecase def foo[C]: A
      |   */
      |  def foo[B]: A => B
      |}
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, _, List(trt: Trait), _, _) =>
          val List(foo: Def) = trt.members

          val returnValue = foo.returnValue match {
            case ref: TypeReference => ref.title
            case _ =>
              assert(
                false,
                "Incorrect return value after usecase transformation"
              )
              ""
          }

          assert(
            foo.typeParams.nonEmpty,
            "Type parameters were incorrectly stripped by usecase"
          )

          assert(foo.typeParams.head == "C", "Incorrectly switched tparam")
          assert(returnValue == "A", "Incorrect return type after usecase")

          assert(foo.name == "foo", s"Incorrect name after transform: ${foo.name}")
      }
    }
  }

  @Test def checkIterator = {
    val sources =
      "./scala-scala/src/library/scala/collection/Iterator.scala" :: Nil

    checkFiles(sources) { packages =>
      // success if typer throws no errors! :)
    }
  }
}
