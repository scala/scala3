package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model._
import model.internal._
import model.references._
import util.syntax._

class UsecaseFromSourceTest extends UsecaseBase with CheckFromSource
class UsecaseFromTastyTest extends UsecaseBase with CheckFromTasty

abstract class UsecaseBase extends DottyDocTest {
  @Test def simpleUsecase = {
    val source = SourceUtil.makeTemp(
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

    val className = "scala.Test"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
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
    val source = SourceUtil.makeTemp(
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

    val className = "scala.Test"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
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
    val source = SourceUtil.makeTemp(
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

    val className = "scala.Test"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
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
    val source = SourceUtil.makeTemp(
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

    val className = "scala.Iterable"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
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
    val source = SourceUtil.makeTemp(
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

    val className = "scala.Iterable"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
      case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
        val List(map: Def) = trt.members
        assert(map.comment.isDefined, "Lost comment in transformations")

        val docstr = ctx.docbase.docstring(map.symbol).get.expandedBody.get
        assert(
          !docstr.contains("@usecase"),
          s"Comment should not contain usecase after stripping, but was:\n$docstr"
        )
      }
    }
  }

  @Test def multipleUseCases: Unit = {
    val source = SourceUtil.makeTemp(
      """
        |package scala
        |
        |trait Test {
        |  /** A first method
        |   *  @usecase def foo(x: Int): Int
        |   *  @usecase def foo(x: Double): Double
        |   */
        |   def foo(x: String): Unit
        |}
      """.stripMargin
    )

    val className = "scala.Test"

    check(className :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
          val List(foo0: Def, foo1: Def) = trt.members
          assertEquals(TypeReference("Int", NoLink("Int", "scala.Int"), Nil), foo0.returnValue)
          assertEquals(TypeReference("Double", NoLink("Double", "scala.Double"), Nil), foo1.returnValue)
      }
    }
  }

  @Test def checkIterator =
    checkFiles("../tests/scala2-library/src/library/scala/collection/Iterator.scala" :: Nil) { case _ =>
      // success if typer throws no errors! :)
    }

  @Test def checkIterableLike =
    checkFiles("../tests/scala2-library/src/library/scala/collection/IterableLike.scala" :: Nil) { case _ =>
      // success if typer throws no errors! :)
    }
}
