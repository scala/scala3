object Test {

  class T[A] { def foo(f: (=> A) => Int) = f(???) }

  def main(args: Array[String]): Unit =
    new T[(Int, Int)].foo((x, y) => 0) // error // error Parameter untupling cannot be used for call-by-name parameters (twice)
}
