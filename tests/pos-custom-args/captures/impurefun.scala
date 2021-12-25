object Test:

  val f: ImpureFunction1[Int, Int] = (x: Int) => x + 1

  val g: Int -> Int = (x: Int) => x + 1

  val h: Int => Int = (x: Int) => x + 1

