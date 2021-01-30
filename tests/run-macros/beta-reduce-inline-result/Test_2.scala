import scala.compiletime.*

object Test {

  inline def dummy1: Int => Int =
    (i: Int) => i + 1

  inline def dummy2: (i: Int) => i.type =
    (i: Int) => i

  inline def dummy3: Int => Int =
    (i: Int) => ???

  inline def dummy4: Int => Int =
    ???

  object I extends (Int => Int) {
    def apply(i: Int): i.type = i
  }

  abstract class II extends (Int => Int) {
    val apply = 123
  }

  inline def dummy5: II =
    (i: Int) => i + 1

  abstract class III extends (Int => Int) {
    def impl(i: Int): Int
    def apply(i: Int): Int = -1
  }

  inline def dummy6: III =
    (i: Int) => i + 1

  abstract class IV extends (Int => Int) {
    def apply(s: String): String
  }

  abstract class V extends IV {
    def apply(s: String): String = "gotcha"
  }

  inline def dummy7: IV =
    { (i: Int) => i + 1 } : V

  def main(argv : Array[String]) : Unit = {
    println(s"compile-time: ${codeOf(Macros.betaReduce(dummy1)(3))}")
    println(s"run-time: ${Macros.betaReduce(dummy1)(3)}")
    println(s"compile-time: ${codeOf(Macros.betaReduce(dummy2)(1))}")
    // paramrefs have to be properly substituted in this case
    println(s"run-time: ${Macros.betaReduce(dummy2)(1)}")

    // ensure the inlined ??? is ascribed type Int so this compiles
    def throwsNotImplemented1 = Macros.betaReduceAdd1(dummy3)(4)
    // ensure we handle cases where the (non-inlineable) function itself needs ascribing
    def throwsNotImplemented2 = Macros.betaReduce(dummy4)(6)

    // make sure paramref types work when inlining is not possible
    println(s"run-time: ${Macros.betaReduce(I)(5)}")

    // -- cases below are non-function types, which are currently not inlined for simplicity but may be in the future
    // (also, this tests that we return something valid when we see a closure that we can't inline)

    // A non-function type with an apply value that can be confused with the apply method.
    println(s"run-time: ${Macros.betaReduce(dummy5)(6)}")

    // should print -1 (without inlining), because the apparent apply method actually
    // has nothing to do with the function literal
    println(s"run-time: ${Macros.betaReduce(dummy6)(7)}")

    // the literal does contain the implementation of the apply method, but there are two abstract apply methods
    // in the outermost abstract type
    println(s"run-time: ${Macros.betaReduce(dummy7)(8)}")
  }
}
