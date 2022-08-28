package dotty.tools
package dotc
package printing

import core.*, Contexts.*, Decorators.*, Names.*, Symbols.*
import ast.tpd.*

import org.junit.Test
import org.junit.Assert.*

class PrinterTests extends DottyTest {
  override def initializeCtx(fc: FreshContext) = super.initializeCtx(fc.setSetting(fc.settings.color, "never"))

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
      val bar @ DefDef(_, _, _, _) = tree.find(tree => tree.symbol.name == termName("bar2")).get: @unchecked
      assertEquals("Int & (Boolean | String)", bar.tpt.show)
    }
  }
}
