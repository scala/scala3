import Macros._

object Test {

  def main(args: Array[String]): Unit = {
    println(liftString(LitDSL(1)))
    println(liftCompute(LitDSL(1)))
    println(liftAST(LitDSL(1)))
    println()
    println(liftString(LitDSL(1) + LitDSL(2)))
    println(liftCompute(LitDSL(1) + LitDSL(2)))
    println(liftAST(LitDSL(1) + LitDSL(2)))
    println()
    println(liftString(LitDSL(1) * LitDSL(2)))
    println(liftCompute(LitDSL(1) * LitDSL(2)))
    println(liftAST(LitDSL(1) * LitDSL(2)))
    println()
    println(liftString(LitDSL(1) + LitDSL(3) * LitDSL(4)))
    println(liftCompute(LitDSL(1) + LitDSL(3) * LitDSL(4)))
    println(liftAST(LitDSL(1) + LitDSL(3) * LitDSL(4)))
    println()
    println(liftString(((x: DSL) => LitDSL(2) + x).apply(LitDSL(5))))
    println(liftCompute(((x: DSL) => LitDSL(2) + x).apply(LitDSL(5))))
    println(liftAST(((x: DSL) => LitDSL(2) + x).apply(LitDSL(5))))
    println()
    println(liftString({ val x: DSL = LitDSL(2); x + x }))
    println(liftCompute({ val x: DSL = LitDSL(2); x + x }))
    println(liftAST({ val x: DSL = LitDSL(2); x + x }))
  }

}
