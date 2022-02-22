package dotty.tools.dotc.printing

import dotty.tools.DottyTest
import dotty.tools.dotc.ast.{Trees,tpd}
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
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
      val bar @ Trees.DefDef(_, _, _, _) = tree.find(tree => tree.symbol.name == termName("bar2")).get
      assertEquals("Int & (Boolean | String)", bar.tpt.show)
    }
  }
}
