package test
import dotty.tools.dotc.ast.UntypedTrees.untpd._

object showTree extends ParserTest {

  def main(args: Array[String]): Unit = {
    for (arg <- args) {
      val tree: Tree = parse(arg)
      println("result = "+tree.show)
    }
  }
}