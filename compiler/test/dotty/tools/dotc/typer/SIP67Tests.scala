package dotty.tools.dotc.typer

import dotty.tools.DottyTest
import dotty.tools.dotc.core.Contexts.*

import org.junit.Test
import org.junit.Assert.fail

class SIP67Tests extends DottyTest:

  @Test
  def sip67test1: Unit =
    val source = """
    import scala.language.strictEquality
    enum Foo:
      case Bar

    val _ =
      (??? : Foo) match
        case Foo.Bar =>
    """
    val ctx = checkCompile("typer", source)((_, ctx) => ())

    if ctx.reporter.hasErrors then
      fail("Unexpected compilation errors were reported")

  @Test
  def sip67test2: Unit =
    val source = """
    import scala.language.strictEquality

    sealed trait Foo

    object Foo:
      case object Bar extends Foo

    val _ =
      (??? : Foo) match
        case Foo.Bar =>
    """
    val ctx = checkCompile("typer", source)((_, ctx) => ())
    if ctx.reporter.hasErrors then
      fail("Unexpected compilation errors were reported")
