object Test {
  val List(x, y, z: _ *,
  ) = 42 :: 17 :: Nil: @unchecked
  def main(args: Array[String]): Unit = {
    Console.println(x)
    Console.println(y)
  }
}
