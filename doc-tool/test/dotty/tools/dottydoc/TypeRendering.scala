package dotty.tools
package dottydoc

import dotty.tools.dotc.util.SourceFile
import dotty.tools.dottydoc.model._
import dotty.tools.dottydoc.model.internal._
import dotty.tools.dottydoc.model.references._

import org.junit.Test
import org.junit.Assert.{assertTrue, fail}

class TypeRenderingTest extends DottyDocTest {
  @Test def renderImplicitFunctionType = {
    val source = new SourceFile(
      "ImplicitFunctionType.scala",
      """
      |package scala
      |
      |trait Test {
      |  def a: implicit Int => Int = ???
      |  def b(x: implicit Int => Int) = ???
      |  type c = implicit Int => Int
      |}
      """.stripMargin
    )

    def checkImplicitFunctionType(ref: Reference) = ref match {
      case FunctionReference(_, _, isImplicit) =>
        assertTrue("Should be an implicit function type", isImplicit)
      case _ =>
        fail("Unexpected: " + ref)
    }

    checkSources(source :: Nil) { packages =>
      packages("scala") match {
        case PackageImpl(_, _, _, List(trt: Trait), _, _, _, _) =>
          val List(a: Def, b: Def, c: TypeAlias) = trt.members.sortBy(_.name)
          checkImplicitFunctionType(a.returnValue)
          b.paramLists.head.list.head match {
            case NamedReference("x", ref, _, _) =>
              checkImplicitFunctionType(ref)
            case _ =>
              fail("Unexpected: " + b.paramLists.head.list.head)
          }
          checkImplicitFunctionType(c.alias.get)
      }
    }
  }
}
