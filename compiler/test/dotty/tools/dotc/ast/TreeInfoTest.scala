package dotty.tools
package dotc
package ast

import org.junit.Test
import core.Names.*
import core.StdNames.nme
import core.Symbols.*
import org.junit.Assert.*
import core.Contexts.Context

class TreeInfoTest extends DottyTest {

  import tpd.*

  @Test
  def testDefPath: Unit = checkCompile("typer", "class A { def bar = { val x = { val z = 0; 0} }} ") {
    (tree, context) =>
      given Context = context
      val xTree = tree.find(tree => tree.symbol.name == termName("x")).get
      val path = defPath(xTree.symbol, tree)
      assertEquals(Vector(
        ("PackageDef", nme.EMPTY_PACKAGE),
        ("TypeDef", typeName("A")),
        ("Template", termName("<local A>")),
        ("DefDef", termName("bar")),
        ("Block", NoSymbol.name),
        ("ValDef", termName("x"))
      ), path.map(x => (x.productPrefix, x.symbol.name)))
  }

  @Test
  def localSymsHandlesMoreThanVector1Width: Unit =
    val stats = (0 to 32).map(i => s"val x$i = $i").mkString("\n")
    checkCompile("typer", s"class A { def bar = { $stats; x32 } }") {
      (tree, context) =>
        given Context = context
        val bar = tree.find(_.symbol.name == termName("bar")).get.asInstanceOf[DefDef]
        val block = bar.rhs.asInstanceOf[Block]
        val syms = localSyms(block.stats)
        assertEquals(33, syms.length)
        assertEquals((0 to 32).map(i => termName(s"x$i")).toVector, syms.map(_.name))
    }
}
