object Test {
  class T[A] { def foo(f: (=> A) => Int) = f(???) }

  def main(args: Array[String]): Unit = {
    new T[(Int, Int)].foo((ii) => 0)
    //new T[(Int, Int)].foo((x, y) => 0) // not allowed anymore
  }
}
