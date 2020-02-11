import scala.compiletime._

object Test {
  
  inline def dummy1: Int => Int =
    (i: Int) => i + 1

  inline def dummy2: (i: Int) => i.type =
    (i: Int) => i

  inline def dummy3: Int => Int =
    (i: Int) => ???

  inline def dummy4: Int => Int =
    ???

  def main(argv : Array[String]) : Unit = {
    println(code"compile-time: ${Macros.betaReduce(dummy1)(3)}")
    println(s"run-time: ${Macros.betaReduce(dummy1)(3)}")
    println(code"compile-time: ${Macros.betaReduce(dummy2)(1)}")
    // paramrefs have to be properly substituted in this case
    println(s"run-time: ${Macros.betaReduce(dummy2)(1)}")

    // ensure the inlined ??? is ascribed type Int so this compiles
    def throwsNotImplemented1 = Macros.betaReduceAdd1(dummy3)(4)
    // ensure we handle cases where the (non-inlineable) function itself needs ascribing
    def throwsNotImplemented2 = Macros.betaReduce(dummy4)(6)

    // make sure paramref types work when inlining is not possible
    println(s"run-time: ${Macros.betaReduce(Predef.identity)(5)}")
  }
}

