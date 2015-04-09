package test
import dotty.tools.dotc._
import ast.Trees._
import ast.desugar
import ast.desugar._
import typer.Mode

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
