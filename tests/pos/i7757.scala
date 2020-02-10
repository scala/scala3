val m: Map[Int, String] = ???
val _ = m.map((a, b) => a + b.length)

trait Foo:
  def g(f: ((Int, Int)) => Int): Int = 1
  def g(f: ((Int, Int)) => (Int, Int)): String = "2"

@main def Test =
  val m: Foo = ???
  m.g((x: Int, b: Int) => (x, x))