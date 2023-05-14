package dotty.tools.dotc.core

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.*

import org.junit.Assert._
import org.junit.Test

import dotty.tools.DottyTest

class SealedDescendantsTest extends DottyTest {

  @Test
  def zincIssue979: Unit =
    val source =
      """
      sealed trait Z
      sealed trait A extends Z
      class B extends A
      class C extends A
      class D extends A
      """

    expectedDescendents(source, "Z",
      "Z" ::
      "A" ::
      "B" ::
      "C" ::
      "D" :: Nil
    )
  end zincIssue979

  @Test
  def enumOpt: Unit =
    val source =
      """
      enum Opt[+T] {
        case Some(t: T)
        case None
      }
      """

    expectedDescendents(source, "Opt",
      "Opt"       ::
      "Some"      ::
      "None.type" :: Nil
    )
  end enumOpt

  @Test
  def hierarchicalSharedChildren: Unit =
    // Q is a child of both Z and A and should appear once
    // X is a child of both A and Q and should appear once
    val source =
      """
      sealed trait Z
      sealed trait A extends Z
      sealed trait Q extends A with Z
      trait X extends A with Q
      case object Y extends Q
      """

    expectedDescendents(source, "Z",
      "Z"      ::
      "A"      ::
      "Q"      ::
      "X"      ::
      "Y.type" :: Nil
    )
  end hierarchicalSharedChildren

  @Test
  def hierarchicalSharedChildrenB: Unit =
    val source =
      """
      sealed trait Z
      case object A extends Z with D with E
      sealed trait B extends Z
      trait C extends B
      sealed trait D extends B
      sealed trait E extends D
      """

    expectedDescendents(source, "Z",
      "Z"      ::
      "A.type" ::
      "B"      ::
      "C"      ::
      "D"      ::
      "E"      :: Nil
    )
  end hierarchicalSharedChildrenB

  def expectedDescendents(source: String, root: String, expected: List[String]) =
    exploreRoot(source, root) { rootCls =>
      val descendents = rootCls.sealedDescendants.map(sym => s"${sym.name}${if (sym.isTerm) ".type" else ""}")
      assertEquals(expected.toString, descendents.toString)
    }

  def exploreRoot(source: String, root: String)(op: Context ?=> ClassSymbol => Unit) =
    val source0 = source.linesIterator.map(_.trim).mkString("\n|")
    val source1 = s"""package testsealeddescendants
                     |$source0""".stripMargin
    checkCompile("typer", source1) { (_, context) =>
      given Context = context
      op(requiredClass(s"testsealeddescendants.$root"))
    }
}
