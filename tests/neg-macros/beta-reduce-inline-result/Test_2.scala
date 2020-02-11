
object Test {
  
  inline def dummy1: Int => Int =
    (i: Int) => i + 1

  inline def dummy2: Int => Int =
    (i: Int) => ???

  val x1: Int = Macros.betaReduce(dummy1)(3)
  val x2: 4   = Macros.betaReduce(dummy1)(3) // error
}

