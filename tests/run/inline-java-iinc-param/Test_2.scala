//> using options -opt -opt-inline:** -Yopt-inline-heuristics:everything

// The javac-compiled `A_1.f` increments its parameter with IINC. When inlined,
// that parameter must not share the caller's local `a`.
object Test {
  def g(a: Int): Int = A_1.f(a) + a
  def main(args: Array[String]): Unit = assert(g(10) == 25, g(10))
}
