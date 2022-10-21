trait Test {

  val x: Int -> Int // error
  val y: Int ?-> Int // error

  val z: *.type // error

  val b: ImpureFunction1[Int, Int] // error

  val a: {z} String // error
}  // error
