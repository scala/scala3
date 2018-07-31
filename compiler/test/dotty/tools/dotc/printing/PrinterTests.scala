package dotty.tools.dotc.printing

import dotty.tools.DottyTest
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Symbols._
import org.junit.Assert.assertEquals
import org.junit.Test

class PrinterTests extends DottyTest {
  import tpd._

  @Test
  def packageObject: Unit = {
    val source = """
      package object foo {
        def bar: Int = 1
      }
    """

    checkCompile("frontend", source) { (tree, context) =>
      implicit val ctx = context
      val bar = tree.find(tree => tree.symbol.name == termName("bar")).get
      assertEquals("package object foo", bar.symbol.owner.show)
    }
  }
}
