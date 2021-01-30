object Test {
  trait A {
    type X = String
  }
  trait B {
    type X = Int
  }

  def foo: A & B = o
  given o: (A & B) = foo

  def xToString(x: o.X): String = x  // error

  def intToString(i: Int): String = xToString(i)

  def main(args: Array[String]): Unit = {
    val z: String = intToString(1)
  }
}