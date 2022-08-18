package dotty.tools
package dotc
package parsing

import core._, ast._

object desugarPackage extends DeSugarTest {

  def test() = {
    reset()
    val start = System.nanoTime()
    val startNodes = Trees.ntrees
    parseDir("./src")
    val ms1 = (System.nanoTime() - start)/1000000
    val nodes = Trees.ntrees
    val buf = parsedTrees map desugarTree
    val ms2 = (System.nanoTime() - start)/1000000
    println(s"$parsed files parsed in ${ms1}ms, ${nodes - startNodes} nodes desugared in ${ms2-ms1}ms, total trees created = ${Trees.ntrees - startNodes}")
    ctx.reporter.printSummary()
  }

  def main(args: Array[String]): Unit = {
//    parse("/Users/odersky/workspace/scala/src/compiler/scala/tools/nsc/doc/model/ModelFactoryTypeSupport.scala")
    for (i <- 0 until 10) test()
  }
}
