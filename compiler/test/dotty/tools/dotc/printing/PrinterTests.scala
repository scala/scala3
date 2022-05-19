package dotty.tools
package dotc
package printing

import ast.{ Trees, tpd }
import core.Names._
import core.Symbols._
import core.Decorators._
import dotty.tools.dotc.core.Contexts.Context

import org.junit.Assert.assertEquals
import org.junit.Test

class PrinterTests extends DottyTest {

  private def newContext = {
    initialCtx.setSetting(ctx.settings.color, "never")
  }
  ctx = newContext

  import tpd._

  @Test
  def packageObject: Unit = {
    val source = """
      package object foo {
        def bar: Int = 1
      }
    """

    checkCompile("typer", source) { (tree, context) =>
      given Context = context
      val bar = tree.find(tree => tree.symbol.name == termName("bar")).get
      assertEquals("package object foo", bar.symbol.owner.show)
    }
  }

  @Test
  def tpTreeInfixOps: Unit = {
    val source = """
      |class &&[T,U]
      |object Foo {
      |  def bar1: Int && (Boolean | String) = ???
      |  def bar2: Int & (Boolean | String) = ???
      |}
    """.stripMargin

    checkCompile("typer", source) { (tree, context) =>
      given Context = context
      val bar @ Trees.DefDef(_, _, _, _) = tree.find(tree => tree.symbol.name == termName("bar2")).get: @unchecked
      assertEquals("Int & (Boolean | String)", bar.tpt.show)
    }
  }

  @Test def string: Unit = assertEquals("foo", i"${"foo"}")

  import core.Flags._
  @Test def flagsSingle: Unit      = assertEquals("final", i"$Final")
  @Test def flagsSeq: Unit         = assertEquals("<static>, final", i"${Seq(JavaStatic, Final)}%, %")
  @Test def flagsTuple: Unit       = assertEquals("(<static>,final)", i"${(JavaStatic, Final)}")
  @Test def flagsSeqOfTuple: Unit  = assertEquals("(final,given), (private,lazy)", i"${Seq((Final, Given), (Private, Lazy))}%, %")

  class StorePrinter extends config.Printers.Printer:
    var string: String = "<never set>"
    override def println(msg: => String) = string = msg

  @Test def testShowing: Unit =
    val store = StorePrinter()
    (JavaStatic | Final).showing(i"flags=$result", store)
    assertEquals("flags=final <static>", store.string)

  @Test def TestShowingWithOriginalType: Unit =
    val store = StorePrinter()
    (JavaStatic | Final).showing(i"flags=${if result.is(Private) then result &~ Private else result | Private}", store)
    assertEquals("flags=private final <static>", store.string)
}
