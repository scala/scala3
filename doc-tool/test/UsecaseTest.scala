package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model._
import model.internal._
import model.references._
import util.syntax._

class UsecaseTest extends DottyDocTest {
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
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
          val List(foo: Def) = trt.members

          assert(foo.comment.isDefined, "Lost comment in transformations")

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
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
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
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
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

  @Test def expandColl = {
    val source = new SourceFile(
      "ExpandColl.scala",
      """
      |package scala
      |
      |/** The trait $Coll
      | *
      | *  @define Coll Iterable
      | */
      |trait Iterable[A] {
      |  /** Definition with a "disturbing" signature
      |   *
      |   *  @usecase def map[B](f: A => B): $Coll[B]
      |   */
      |  def map[B, M[B]](f: A => B): M[B] = ???
      |}
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
      case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
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
          returnValue == "Iterable",
          "Incorrect return type after usecase transformation"
        )
      }
    }
  }

  @Test def checkStripping = {
    val source = new SourceFile(
      "CheckStripping.scala",
      """
      |package scala
      |
      |/** The trait $Coll
      | *
      | *  @define Coll Iterable
      | */
      |trait Iterable[A] {
      |  /** Definition with a "disturbing" signature
      |   *
      |   *  @usecase def map[B](f: A => B): $Coll[B]
      |   */
      |  def map[B, M[B]](f: A => B): M[B] = ???
      |}
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
      case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
        val List(map: Def) = trt.members
        assert(map.comment.isDefined, "Lost comment in transformations")

        val docstr = ctx.docbase.docstring(map.symbol).get.raw
        assert(
          !docstr.contains("@usecase"),
          s"Comment should not contain usecase after stripping, but was:\n$docstr"
        )
      }
    }
  }

  @Test def checkIterator =
    checkFiles("../scala-library/src/library/scala/collection/Iterator.scala" :: Nil) { _ =>
      // success if typer throws no errors! :)
    }

  @Test def checkIterableLike =
    checkFiles("../scala-library/src/library/scala/collection/IterableLike.scala" :: Nil) { _ =>
      // success if typer throws no errors! :)
    }
}
