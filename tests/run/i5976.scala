object Test extends App {
  def f(i: => Int) = i + i
  implicit def ups(f: ((=>Int) => Int)): (Int => Int) = x => f(x)
  val res = List(42).map(f)

  val g: (=> Int) => Int = f
  val h: Int => Int = g

  assert(res == List(84))
}