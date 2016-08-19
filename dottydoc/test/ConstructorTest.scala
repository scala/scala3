package dotty.tools
package dottydoc

import org.junit.Test
import org.junit.Assert._

import dotc.util.SourceFile
import model._
import model.internal._
import model.references._

class Constructors extends DottyTest {
  @Test def singleClassConstructor = {
    val source = new SourceFile (
      "Class.scala",
      """
      |package scala
      |
      |class Class(val str: String)
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(cls: Class), _, _) =>
          cls.constructors.headOption match {
            case Some(ParamListImpl(NamedReference("str", _,  false, false) :: Nil, false) :: Nil) =>
              // success!
            case _ => assert(false, s"Incorrect constructor found: ${cls.constructors}")
          }
      }
    }
  }

  @Test def constructorPlusImplicitArgList = {
    val source = new SourceFile (
      "Class.scala",
      """
      |package scala
      |
      |class Class(val str1: String)(implicit str2: String)
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(cls: Class), _, _) =>
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
    val source = new SourceFile (
      "Class.scala",
      """
      |package scala
      |
      |class Class(val str1: String)(val str2: String)(implicit str3: String)
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(cls: Class), _, _) =>
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
    val source = new SourceFile (
      "Class.scala",
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

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(cls: Class), _, _) =>
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
    val source = new SourceFile (
      "Class.scala",
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

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(cls: CaseClass, obj: Object), _, _) =>
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
    val source = new SourceFile (
      "Trait.scala",
      """
      |package scala
      |
      |trait Trait(val main: String)
      """.stripMargin
    )

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(trt: Trait), _, _) =>
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

  @Test def testJson = {
    val actualSource =
      """
      |package scala
      |
      |trait Trait(val main: String)
      |class Class(val main: String)
      |case class CaseClass(main: String)
      """.stripMargin

    val source = new SourceFile ("JsonTest.scala", actualSource)

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, List(cc: CaseClass, _, cls: Class, trt: Trait), _, _) =>
          import model.json._
          lazy val incorrectJson = s"The json generated for:\n$actualSource\n\nIs not correct"
          assert(cc.json.contains(s""""constructors":[[{"list":[{"title":"main""""), incorrectJson)
          assert(cls.json.contains(s""""constructors":[[{"list":[{"title":"main""""), incorrectJson)
          assert(trt.json.contains(s""""traitParams":[{"list":[{"title":"main""""), incorrectJson)
      }
    }
  }
}
