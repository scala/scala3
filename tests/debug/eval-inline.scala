object Test:
  def main(args: Array[String]): Unit =
    inline val y = 2
    println("Hello, World!")

  inline def m1: Int = 42
  inline def m2(inline x: Int): Int = x
  private inline val x = 1

  inline def test1(inline x: Int): Test = Test(x)
  inline def test2: Int = test1(42).x
  case class Test(x: Int)
