package test
import dotty.tools.dotc.ast.untpd._

object showTree extends ParserTest {

  object DeSugar extends TreeTransformer {
    override def transform(tree: Tree) = {
      val tree1 = desugar(tree, Mode.Expr)
      if (tree1 ne tree) {
        println(s"repeat desugar ${tree1.getClass}")
        transform(tree1)
      }
      else tree1 match {
        case PostfixOp(od, op) => PostfixOp(transform(od), op)
        case tree1 => super.transform(tree1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    for (arg <- args) {
      val tree: Tree = parse(arg)
      println("result = "+tree.show)
      println("desugared = "+DeSugar.transform(tree).show)
    }
  }
}