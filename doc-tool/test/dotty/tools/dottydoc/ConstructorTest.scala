package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model._
import model.internal._
import model.references._

class ConstructorsFromSourceTest extends ConstructorsBase with CheckFromSource
class ConstructorsFromTastyTest extends ConstructorsBase with CheckFromTasty

abstract class ConstructorsBase extends DottyDocTest {
  @Test def singleClassConstructor = {
    val source = SourceUtil.makeTemp(
      """
      |package scala
      |
      |class Class(val str: String)
      """.stripMargin
    )

    val tastyFile = "scala/Class.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(cls: Class), _, _, _, _) =>
          cls.constructors.headOption match {
            case Some(ParamListImpl(NamedReference("str", _,  false, false) :: Nil, false) :: Nil) =>
              // success!
            case _ => assert(false, s"Incorrect constructor found: ${cls.constructors}")
          }
      }
    }
  }

  @Test def constructorPlusImplicitArgList = {
    val source = SourceUtil.makeTemp(
      """
      |package scala
      |
      |class Class(val str1: String)(implicit str2: String)
      """.stripMargin
    )

    val tastyFile = "scala/Class.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _,  _, List(cls: Class), _, _, _, _) =>
          cls.constructors match {
            case (
              ParamListImpl(NamedReference("str1", _,  false, false) :: Nil, false) ::
              ParamListImpl(NamedReference("str2", _, false, false) :: Nil, true) :: Nil
            ) :: Nil =>
              // success!
            case _ => assert(false, s"Incorrect constructor found: ${cls.constructors}")
          }
      }
    }
  }

  @Test def multipleArgumentListsForConstructor = {
    val source = SourceUtil.makeTemp(
      """
      |package scala
      |
      |class Class(val str1: String)(val str2: String)(implicit str3: String)
      """.stripMargin
    )

    val tastyFile = "scala/Class.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(cls: Class), _, _, _, _) =>
          cls.constructors match {
            case (
              ParamListImpl(NamedReference("str1", _,  false, false) :: Nil, false) ::
              ParamListImpl(NamedReference("str2", _, false, false) :: Nil, false) ::
              ParamListImpl(NamedReference("str3", _, false, false) :: Nil, true) :: Nil
            ) :: Nil =>
              // success!
            case _ => assert(false, s"Incorrect constructor found: ${cls.constructors}")
          }
      }
    }
  }

  @Test def multipleConstructors = {
    val source = SourceUtil.makeTemp(
      """
      |package scala
      |
      |class Class(val main: String) {
      |  def this(alt1: Int) =
      |    this("String")
      |
      |  def this(alt2: List[String]) =
      |    this(alt2.head)
      |}
      """.stripMargin
    )

    val tastyFile = "scala/Class.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(cls: Class), _, _, _, _) =>
          cls.constructors match {
            case (
              ParamListImpl(NamedReference("main", _,  false, false) :: Nil, false) :: Nil
            ) :: (
              ParamListImpl(NamedReference("alt1", _, false, false) :: Nil, false) :: Nil
            ) :: (
              ParamListImpl(NamedReference("alt2", _, false, false) :: Nil, false) :: Nil
            ) :: Nil =>
              // success!
            case _ =>
              assert(
                false,
                s"""Incorrect constructor found:\n${cls.constructors.mkString("\n")}"""
              )
          }
      }
    }
  }

  @Test def multipleConstructorsCC = {
    val source = SourceUtil.makeTemp(
      """
      |package scala
      |
      |case class Class(val main: String) {
      |  def this(alt1: Int) =
      |    this("String")
      |
      |  def this(alt2: List[String]) =
      |    this(alt2.head)
      |}
      """.stripMargin
    )

    val tastyFile = "scala/Class.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(cls: CaseClass, obj: Object), _, _, _, _) =>
          cls.constructors match {
            case (
              ParamListImpl(NamedReference("main", _,  false, false) :: Nil, false) :: Nil
            ) :: (
              ParamListImpl(NamedReference("alt1", _, false, false) :: Nil, false) :: Nil
            ) :: (
              ParamListImpl(NamedReference("alt2", _, false, false) :: Nil, false) :: Nil
            ) :: Nil =>
              // success!
            case _ =>
              println(obj.members.map(x => x.kind + " " + x.name))
              assert(
                false,
                s"""Incorrect constructor found:\n${cls.constructors.mkString("\n")}"""
              )
          }
      }
    }
  }

  @Test def traitParameters = {
    val source = SourceUtil.makeTemp(
      """
      |package scala
      |
      |trait Trait(val main: String)
      """.stripMargin
    )

    val tastyFile = "scala/Trait.tasty"

    check(tastyFile :: Nil, source :: Nil) { (ctx, packages) =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
          trt.traitParams match {
            case ParamListImpl(NamedReference("main", _,  false, false) :: Nil, false) :: Nil =>
            case _ =>
              assert(
                false,
                s"""Incorrect constructor found:\n${trt.traitParams.mkString("\n")}"""
              )
          }
      }
    }
  }
}
