package dotty.tools.dotc.typer

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.*

import org.junit.Test
import org.junit.Assert.fail

class SIP67Tests extends DottyTest:

  private def checkNoErrors(source: String): Unit =
    val ctx = checkCompile("typer", source)((_, _) => ())
    if ctx.reporter.hasErrors then
      fail("Unexpected compilation errors were reported")
  
  @Test
  def sip67test1: Unit =
    checkNoErrors:
      """
      import scala.language.strictEquality
      import scala.language.experimental.strictEqualityPatternMatching
      enum Foo:
        case Bar

      val _ =
        (??? : Foo) match
          case Foo.Bar =>
      """
  @Test
  def sip67test2: Unit =
    checkNoErrors:
      """
      import scala.language.strictEquality
      import scala.language.experimental.strictEqualityPatternMatching

      sealed trait Foo

      object Foo:
        case object Bar extends Foo

      val _ =
        (??? : Foo) match
          case Foo.Bar =>
      """
