package dotty.tools.dotc.qualified_types

import dotty.tools.DottyTest
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context

import org.junit.Assert.assertEquals
import org.junit.Test

class ENodeTest extends QualifiedTypesTest:

  def checkFromToTree(exprString: String, resultString: String): Unit =
    checkCompileExpr(s"val v = $exprString"): stats =>
      val tree1: tpd.Tree = getValDef(stats, "v").rhs
      val enode: ENode = ENode.fromTree(tree1).get
      assertStringEquals(resultString, enode.show)
      val tree2: tpd.Tree = enode.toTree()
      assertStringEquals(tree1.show, tree2.show)

  @Test def testFromToTree1() =
    checkFromToTree(
      "(param0: Int) => param0",
      "(_: Int) => eparam0"
    )

  @Test def testFromToTree2() =
    checkFromToTree(
      "(param0: Int) => param0 + 1",
      "(_: Int) => eparam0 + 1"
    )

  @Test def testFromToTree3() =
    // ENode.fromTree and ENode#toTree do not perform constant folding or
    // normalization. This is only done when adding E-Nodes to an E-Graph.
    checkFromToTree(
      "(param0: Int) => param0 + 1 + 1",
      "(_: Int) => eparam0 + 1 + 1"
    )

  @Test def testFromToTree4() =
    checkFromToTree(
      "(param0: Int) => (param1: Int) => param0 + param1",
      // In De Bruijn notation the outermost parameter is param1 and the
      // innermost param0
      "(_: Int) => (_: Int) => eparam1 + eparam0"
    )

  @Test def testFromToTree5() =
    checkFromToTree(
      "(param0: Int, param1: Int) => param0 + param1",
      // Same for paramter lists with multiple parameters: the outermost
      // parameter is param1 and the innermost param0
      "(_: Int, _: Int) => eparam1 + eparam0"
    )

  @Test def testFromToTree6() =
    checkFromToTree(
      "(param0: Int, param1: Int) => (param2: Int, param3: Int) => param0 + param1 + param2 + param3",
      "(_: Int, _: Int) => (_: Int, _: Int) => eparam3 + eparam2 + eparam1 + eparam0"
    )
