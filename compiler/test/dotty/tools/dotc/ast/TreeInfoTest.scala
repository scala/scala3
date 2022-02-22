package dotty.tools
package dotc
package ast

import org.junit.Test
import core.Names._
import core.StdNames.nme
import core.Symbols._
import org.junit.Assert._
import core.Contexts.Context

class TreeInfoTest extends DottyTest {

  import tpd._

  @Test
  def testDefPath: Unit = checkCompile("typer", "class A { def bar = { val x = { val z = 0; 0} }} ") {
    (tree, context) =>
      given Context = context
      val xTree = tree.find(tree => tree.symbol.name == termName("x")).get
      val path = defPath(xTree.symbol, tree)
      assertEquals(List(
        ("PackageDef", nme.EMPTY_PACKAGE),
        ("TypeDef", typeName("A")),
        ("Template", termName("<local A>")),
        ("DefDef", termName("bar")),
        ("Block", NoSymbol.name),
        ("ValDef", termName("x"))
      ), path.map(x => (x.productPrefix, x.symbol.name)))
  }
}
