package dotty.tools
package dotc
package parsing

import ast.Trees._
import ast.desugar
import ast.desugar._
import core.Mode

object showTree extends DeSugarTest {

  import dotty.tools.dotc.ast.untpd._

  import Mode._

  def test(arg: String) = {
    val tree: Tree = parse(arg)
    println("result = " + tree.show)
    println("desugared = " + DeSugar.transform(tree).show)
  }

  def main(args: Array[String]): Unit = {
    test("src/dotty/tools/dotc/core/Types.scala")
    for (arg <- args) test(arg)
  }
}
