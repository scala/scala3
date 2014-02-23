package test
import dotty.compiler.internal._
import ast.Trees._
import ast.desugar
import ast.desugar._
import typer.Mode

object showTree extends DeSugarTest {

  import dotty.compiler.internal.ast.untpd._

  import Mode._

  def test(arg: String) = {
    val tree: Tree = parse(arg)
    println("result = "+tree.show)
    println("desugared = "+DeSugar.transform(tree).show)
  }

  def main(args: Array[String]): Unit = {
    test("src/dotty/compiler/internal/core/Types.scala")
    for (arg <- args) test(arg)
  }
}
