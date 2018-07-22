object Test {

  object double extends (Int => Int) {
    transparent def apply(x: Int) = x * 2
  }

  transparent def twice(f: Int => Int): Int = f(f(2))

  val res = twice(double) // inlined as 8: Int

}