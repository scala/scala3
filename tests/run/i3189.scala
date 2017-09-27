class Test[A](action: A => A) {
  def this() = this(a => a)
  def go(x: A) = action(x)
}

object Test extends App {
  assert(new Test[Int]().go(3) == 3)
}

