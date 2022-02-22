package dotty.tools
package dotc
package parsing


object showTree extends DeSugarTest {

  import dotty.tools.dotc.ast.untpd._


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
